#include "zir/TextParser.hpp"

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <cstring>
#include <functional>
#include <stdexcept>
#include <unordered_map>
#include <vector>

#include "zir/Builder.hpp"

// Hand-written recursive-descent parser for the textual form Printer.cpp
// writes. See include/zir/TextParser.hpp for what round-trip guarantee this
// actually gives (content, not byte-for-byte with arbitrary whitespace).

namespace zust::zir {

    namespace {

        enum class Tok {
            Eof,
            Ident,
            Percent,
            At,
            Caret,
            Number,
            ByteString,
            LParen,
            RParen,
            LBrace,
            RBrace,
            LBracket,
            RBracket,
            Comma,
            Colon,
            Equals,
            Arrow,
            Ellipsis,
        };

        struct Token {
            Tok kind = Tok::Eof;
            std::string text;
        };

        // A parse-error escape hatch, caught only at TextParser::parse's top
        // level -- never propagates to the caller (CONVENTIONS.md: the
        // compiler never crashes on bad input; this is a frontend, not an
        // internal invariant).
        struct ParseError : std::runtime_error {
            explicit ParseError(const std::string &msg) : std::runtime_error(msg) {}
        };

        std::vector<Token> tokenize(std::string_view src) {
            std::vector<Token> toks;
            std::size_t pos = 0;
            auto peek = [&](std::size_t o = 0) -> char { return pos + o < src.size() ? src[pos + o] : '\0'; };

            auto scanQuoted = [&](bool hexUnescape) -> std::string {
                // Assumes the opening '"' is the current character.
                pos++;  // consume opening quote
                std::string out;
                while (pos < src.size() && src[pos] != '"') {
                    if (hexUnescape && src[pos] == '\\' && pos + 2 < src.size() &&
                        std::isxdigit((unsigned char)src[pos + 1]) && std::isxdigit((unsigned char)src[pos + 2])) {
                        auto hexVal = [](char c) -> int {
                            if (c >= '0' && c <= '9')
                                return c - '0';
                            if (c >= 'A' && c <= 'F')
                                return c - 'A' + 10;
                            return c - 'a' + 10;
                        };
                        out += static_cast<char>(hexVal(src[pos + 1]) * 16 + hexVal(src[pos + 2]));
                        pos += 3;
                    } else {
                        out += src[pos];
                        pos++;
                    }
                }
                if (pos >= src.size())
                    throw ParseError("unterminated quoted string");
                pos++;  // consume closing quote
                return out;
            };

            while (true) {
                // Skip whitespace and `; ...` line comments.
                while (pos < src.size() &&
                       (src[pos] == ' ' || src[pos] == '\t' || src[pos] == '\r' || src[pos] == '\n'))
                    pos++;
                if (pos < src.size() && src[pos] == ';') {
                    while (pos < src.size() && src[pos] != '\n')
                        pos++;
                    continue;
                }
                if (pos >= src.size())
                    break;

                char c = src[pos];
                if (c == '%') {
                    pos++;
                    std::string name;
                    while (pos < src.size() &&
                           (std::isalnum((unsigned char)src[pos]) || src[pos] == '_' || src[pos] == '.'))
                        name += src[pos++];
                    toks.push_back({Tok::Percent, name});
                } else if (c == '@') {
                    pos++;
                    std::string name;
                    while (pos < src.size() &&
                           (std::isalnum((unsigned char)src[pos]) || src[pos] == '_' || src[pos] == '.'))
                        name += src[pos++];
                    toks.push_back({Tok::At, name});
                } else if (c == '^') {
                    pos++;
                    std::string name;
                    while (pos < src.size() &&
                           (std::isalnum((unsigned char)src[pos]) || src[pos] == '_' || src[pos] == '.'))
                        name += src[pos++];
                    toks.push_back({Tok::Caret, name});
                } else if (c == '-' && peek(1) == '>') {
                    pos += 2;
                    toks.push_back({Tok::Arrow, "->"});
                } else if (c == '.' && peek(1) == '.' && peek(2) == '.') {
                    pos += 3;
                    toks.push_back({Tok::Ellipsis, "..."});
                } else if (c == '(') {
                    pos++;
                    toks.push_back({Tok::LParen, "("});
                } else if (c == ')') {
                    pos++;
                    toks.push_back({Tok::RParen, ")"});
                } else if (c == '{') {
                    pos++;
                    toks.push_back({Tok::LBrace, "{"});
                } else if (c == '}') {
                    pos++;
                    toks.push_back({Tok::RBrace, "}"});
                } else if (c == '[') {
                    pos++;
                    toks.push_back({Tok::LBracket, "["});
                } else if (c == ']') {
                    pos++;
                    toks.push_back({Tok::RBracket, "]"});
                } else if (c == ',') {
                    pos++;
                    toks.push_back({Tok::Comma, ","});
                } else if (c == ':') {
                    pos++;
                    toks.push_back({Tok::Colon, ":"});
                } else if (c == '=') {
                    pos++;
                    toks.push_back({Tok::Equals, "="});
                } else if (c == 'c' && peek(1) == '"') {
                    pos++;  // consume 'c'
                    toks.push_back({Tok::ByteString, scanQuoted(/*hexUnescape=*/true)});
                } else if (c == '"') {
                    toks.push_back({Tok::ByteString, scanQuoted(/*hexUnescape=*/false)});
                } else if (c == '-' || std::isdigit((unsigned char)c)) {
                    std::string num;
                    if (c == '-')
                        num += src[pos++];
                    while (pos < src.size() && std::isdigit((unsigned char)src[pos]))
                        num += src[pos++];
                    if (pos < src.size() && src[pos] == '.') {
                        num += src[pos++];
                        while (pos < src.size() && std::isdigit((unsigned char)src[pos]))
                            num += src[pos++];
                    }
                    if (pos < src.size() && (src[pos] == 'e' || src[pos] == 'E')) {
                        num += src[pos++];
                        if (pos < src.size() && (src[pos] == '+' || src[pos] == '-'))
                            num += src[pos++];
                        while (pos < src.size() && std::isdigit((unsigned char)src[pos]))
                            num += src[pos++];
                    }
                    toks.push_back({Tok::Number, num});
                } else if (std::isalpha((unsigned char)c) || c == '_') {
                    std::string id;
                    while (pos < src.size() && (std::isalnum((unsigned char)src[pos]) || src[pos] == '_'))
                        id += src[pos++];
                    toks.push_back({Tok::Ident, id});
                } else {
                    throw ParseError(std::string("unexpected character '") + c + "'");
                }
            }
            toks.push_back({Tok::Eof, ""});
            return toks;
        }

        // Opcode/predicate keyword tables -- the exact inverse of
        // Printer.cpp's opcodeKeyword/cmpPredKeyword.
        const std::unordered_map<std::string, Opcode> kBinopKeywords = {
            {"add", Opcode::Add},   {"sub", Opcode::Sub},   {"mul", Opcode::Mul},   {"sdiv", Opcode::SDiv},
            {"udiv", Opcode::UDiv}, {"srem", Opcode::SRem}, {"urem", Opcode::URem}, {"and", Opcode::And},
            {"or", Opcode::Or},     {"xor", Opcode::Xor},   {"shl", Opcode::Shl},   {"lshr", Opcode::LShr},
            {"ashr", Opcode::AShr}, {"fadd", Opcode::FAdd}, {"fsub", Opcode::FSub}, {"fmul", Opcode::FMul},
            {"fdiv", Opcode::FDiv},
        };
        const std::unordered_map<std::string, Opcode> kUnopKeywords = {{"neg", Opcode::Neg}, {"not", Opcode::Not}};
        const std::unordered_map<std::string, Opcode> kCastKeywords = {
            {"trunc", Opcode::Trunc},       {"zext", Opcode::ZExt},         {"sext", Opcode::SExt},
            {"fptrunc", Opcode::FPTrunc},   {"fpext", Opcode::FPExt},       {"fptosi", Opcode::FPToSI},
            {"fptoui", Opcode::FPToUI},     {"sitofp", Opcode::SIToFP},     {"uitofp", Opcode::UIToFP},
            {"ptrtoint", Opcode::PtrToInt}, {"inttoptr", Opcode::IntToPtr}, {"bitcast", Opcode::Bitcast},
        };
        const std::unordered_map<std::string, CmpPred> kIcmpPreds = {
            {"eq", CmpPred::Eq},   {"ne", CmpPred::Ne},   {"slt", CmpPred::Slt}, {"sle", CmpPred::Sle},
            {"sgt", CmpPred::Sgt}, {"sge", CmpPred::Sge}, {"ult", CmpPred::Ult}, {"ule", CmpPred::Ule},
            {"ugt", CmpPred::Ugt}, {"uge", CmpPred::Uge},
        };
        const std::unordered_map<std::string, CmpPred> kFcmpPreds = {
            {"oeq", CmpPred::Oeq}, {"one", CmpPred::One}, {"olt", CmpPred::Olt},
            {"ole", CmpPred::Ole}, {"ogt", CmpPred::Ogt}, {"oge", CmpPred::Oge},
        };

        class ParserImpl {
        public:
            explicit ParserImpl(std::string_view src) : toks_(tokenize(src)) {}

            Module parseModule() {
                expectIdent("module");
                std::string sourceName = expectByteString();
                expectIdent("target");
                expect(Tok::Equals, "'='");
                std::string targetName = expectByteString();

                Module m(sourceName, targetName);

                struct PendingBody {
                    FuncId id;
                    std::size_t start;
                    std::size_t end;  // index of the matching '}' (exclusive)
                };

                std::vector<PendingBody> pending;

                while (!at(Tok::Eof)) {
                    if (atIdent("declare")) {
                        parseDeclare(m);
                    } else if (atIdent("fn")) {
                        FuncId id = parseFnHeader(m);
                        std::size_t start = pos_;
                        std::size_t end = skipBalancedBraces();
                        pending.push_back({id, start, end});
                    } else if (at(Tok::At)) {
                        parseGlobal(m);
                    } else {
                        throw ParseError("unexpected top-level token: '" + cur().text + "'");
                    }
                }

                for (const PendingBody &body : pending) {
                    pos_ = body.start;
                    parseFunctionBody(m, m.function(body.id), body.end);
                }

                return m;
            }

        private:
            std::vector<Token> toks_;
            std::size_t pos_ = 0;

            const Token &cur() const { return toks_[pos_]; }

            bool at(Tok k) const { return cur().kind == k; }

            bool atIdent(const std::string &s) const { return cur().kind == Tok::Ident && cur().text == s; }

            void advance() {
                if (pos_ + 1 < toks_.size())
                    pos_++;
            }

            void expect(Tok k, const char *what) {
                if (cur().kind != k)
                    throw ParseError(std::string("expected ") + what + ", got '" + cur().text + "'");
                advance();
            }

            void expectIdent(const std::string &s) {
                if (!atIdent(s))
                    throw ParseError("expected '" + s + "', got '" + cur().text + "'");
                advance();
            }

            std::string expectAnyIdent() {
                if (cur().kind != Tok::Ident)
                    throw ParseError("expected an identifier, got '" + cur().text + "'");
                std::string s = cur().text;
                advance();
                return s;
            }

            std::string expectPercent() {
                if (cur().kind != Tok::Percent)
                    throw ParseError("expected a %value, got '" + cur().text + "'");
                std::string s = cur().text;
                advance();
                return s;
            }

            std::string expectAt() {
                if (cur().kind != Tok::At)
                    throw ParseError("expected an @name, got '" + cur().text + "'");
                std::string s = cur().text;
                advance();
                return s;
            }

            std::string expectCaret() {
                if (cur().kind != Tok::Caret)
                    throw ParseError("expected a ^label, got '" + cur().text + "'");
                std::string s = cur().text;
                advance();
                return s;
            }

            std::string expectNumber() {
                if (cur().kind != Tok::Number)
                    throw ParseError("expected a number, got '" + cur().text + "'");
                std::string s = cur().text;
                advance();
                return s;
            }

            std::string expectByteString() {
                if (cur().kind != Tok::ByteString)
                    throw ParseError("expected a quoted string, got '" + cur().text + "'");
                std::string s = cur().text;
                advance();
                return s;
            }

            // Consumes tokens until (and including) the '}' matching the
            // '{' already consumed by the caller. Returns the index of that
            // closing '}' token. No nested '{'/'}' occur in this grammar
            // (blocks aren't brace-delimited), but depth-counting costs
            // nothing and makes this safe regardless.
            std::size_t skipBalancedBraces() {
                int depth = 1;
                while (depth > 0) {
                    if (at(Tok::Eof))
                        throw ParseError("unterminated function body");
                    if (at(Tok::LBrace))
                        depth++;
                    else if (at(Tok::RBrace))
                        depth--;
                    if (depth > 0)
                        advance();
                }
                std::size_t closeBrace = pos_;
                advance();  // consume the '}'
                return closeBrace;
            }

            TypeId parseType(Module &m) {
                if (at(Tok::LBracket)) {
                    advance();
                    std::uint64_t len = std::stoull(expectNumber());
                    expectIdent("x");
                    TypeId elem = parseType(m);
                    expect(Tok::RBracket, "']'");
                    return m.types().arrayType(elem, len);
                }
                std::string tok = expectAnyIdent();
                if (tok == "void")
                    return m.types().voidType();
                if (tok == "ptr")
                    return m.types().ptrType(m.types().intType(8, true));
                if (tok == "fn") {
                    expect(Tok::LParen, "'('");
                    std::vector<TypeId> params;
                    bool variadic = false;
                    while (!at(Tok::RParen)) {
                        if (at(Tok::Ellipsis)) {
                            variadic = true;
                            advance();
                            break;
                        }
                        params.push_back(parseType(m));
                        if (at(Tok::Comma))
                            advance();
                        else
                            break;
                    }
                    expect(Tok::RParen, "')'");
                    expect(Tok::Arrow, "'->'");
                    TypeId ret = parseType(m);
                    return m.types().fnType(params, ret, variadic);
                }
                if (tok.size() > 1 && (tok[0] == 'i' || tok[0] == 'u' || tok[0] == 'f')) {
                    bool allDigits = true;
                    for (std::size_t i = 1; i < tok.size(); ++i) {
                        if (!std::isdigit((unsigned char)tok[i])) {
                            allDigits = false;
                            break;
                        }
                    }
                    if (allDigits) {
                        std::uint32_t bits = static_cast<std::uint32_t>(std::stoul(tok.substr(1)));
                        if (tok[0] == 'f')
                            return m.types().floatType(bits);
                        return m.types().intType(bits, tok[0] == 'i');
                    }
                }
                throw ParseError("unknown type token: '" + tok + "'");
            }

            void parseGlobal(Module &m) {
                GlobalVar g;
                g.name = expectAt();
                expect(Tok::Equals, "'='");
                if (atIdent("private")) {
                    g.isPrivate = true;
                    advance();
                }
                if (atIdent("constant")) {
                    g.isConstant = true;
                    advance();
                } else if (atIdent("global")) {
                    g.isConstant = false;
                    advance();
                } else {
                    throw ParseError("expected 'constant' or 'global', got '" + cur().text + "'");
                }
                g.type = parseType(m);
                if (at(Tok::ByteString)) {
                    g.hasInit = true;
                    g.initBytes = expectByteString();
                }
                m.addGlobal(g);
            }

            void parseDeclare(Module &m) {
                expectIdent("declare");
                TypeId retTy = parseType(m);
                std::string name = expectAt();
                expect(Tok::LParen, "'('");
                std::vector<TypeId> params;
                bool variadicInParens = false;
                while (!at(Tok::RParen)) {
                    if (at(Tok::Ellipsis)) {
                        variadicInParens = true;
                        advance();
                        break;
                    }
                    params.push_back(parseType(m));
                    if (at(Tok::Comma))
                        advance();
                    else
                        break;
                }
                expect(Tok::RParen, "')'");
                bool isVariadic = false;
                if (atIdent("variadic")) {
                    isVariadic = true;
                    advance();
                }
                TypeId sig = m.types().fnType(params, retTy, variadicInParens);
                m.addFunction(Function(name, sig, /*isExtern=*/true, isVariadic));
            }

            // Parses `fn @name(%p: ty, ...) -> retty {` and registers the
            // Function (no blocks yet); returns its FuncId. The header's
            // own %param names are discarded -- they're redundant with what
            // the entry block re-declares, which is what the printer
            // actually derives this line from.
            FuncId parseFnHeader(Module &m) {
                expectIdent("fn");
                std::string name = expectAt();
                expect(Tok::LParen, "'('");
                std::vector<TypeId> paramTypes;
                while (!at(Tok::RParen)) {
                    expectPercent();
                    expect(Tok::Colon, "':'");
                    paramTypes.push_back(parseType(m));
                    if (at(Tok::Comma))
                        advance();
                    else
                        break;
                }
                expect(Tok::RParen, "')'");
                expect(Tok::Arrow, "'->'");
                TypeId retTy = parseType(m);
                expect(Tok::LBrace, "'{'");
                TypeId sig = m.types().fnType(paramTypes, retTy, false);
                return m.addFunction(Function(name, sig, /*isExtern=*/false, /*isVariadic=*/false));
            }

            // Scans [start, end) for block-header carets ("^label" followed
            // by an optional "(...)" then a ':') to determine block creation
            // order, distinct from the same "^label" syntax used as a bare
            // branch-target operand (which never has a trailing ':').
            std::vector<std::string> scanBlockLabels(std::size_t start, std::size_t end) {
                std::vector<std::string> labels;
                for (std::size_t p = start; p < end;) {
                    if (toks_[p].kind == Tok::Caret) {
                        // The label name is the Caret token's own text
                        // ("^entry" lexes as one token, not '^' + Ident).
                        std::string label = toks_[p].text;
                        std::size_t q = p + 1;
                        if (q < end && toks_[q].kind == Tok::LParen) {
                            int depth = 1;
                            q++;
                            while (q < end && depth > 0) {
                                if (toks_[q].kind == Tok::LParen)
                                    depth++;
                                else if (toks_[q].kind == Tok::RParen)
                                    depth--;
                                q++;
                            }
                        }
                        if (q < end && toks_[q].kind == Tok::Colon) {
                            labels.push_back(label);
                        }
                    }
                    p++;
                }
                return labels;
            }

            void parseFunctionBody(Module &m, Function &fn, std::size_t bodyEnd) {
                std::vector<std::string> labels = scanBlockLabels(pos_, bodyEnd);
                std::unordered_map<std::string, BlockId> labelToId;
                for (const std::string &label : labels)
                    labelToId[label] = fn.addBlock(label);
                if (!labels.empty())
                    fn.setEntry(labelToId[labels.front()]);

                std::unordered_map<std::string, ValueId> valueByName;

                auto resolveBlockRef = [&]() -> BlockRef {
                    std::string label = expectCaret();
                    auto it = labelToId.find(label);
                    if (it == labelToId.end())
                        throw ParseError("reference to undeclared block '^" + label + "'");
                    BlockRef ref;
                    ref.block = it->second;
                    if (at(Tok::LParen)) {
                        advance();
                        while (!at(Tok::RParen)) {
                            std::string vname = expectPercent();
                            auto vit = valueByName.find(vname);
                            if (vit == valueByName.end())
                                throw ParseError("use of undefined value '%" + vname + "'");
                            ref.args.push_back(vit->second);
                            if (at(Tok::Comma))
                                advance();
                            else
                                break;
                        }
                        expect(Tok::RParen, "')'");
                    }
                    return ref;
                };

                auto resolveValue = [&]() -> ValueId {
                    std::string vname = expectPercent();
                    auto it = valueByName.find(vname);
                    if (it == valueByName.end())
                        throw ParseError("use of undefined value '%" + vname + "'");
                    return it->second;
                };

                while (pos_ < bodyEnd) {
                    // Block header.
                    std::string label = expectCaret();
                    BlockId blockId = labelToId.at(label);
                    if (at(Tok::LParen)) {
                        advance();
                        while (!at(Tok::RParen)) {
                            std::string pname = expectPercent();
                            expect(Tok::Colon, "':'");
                            TypeId ty = parseType(m);
                            ValueId v = fn.newValue(ty);
                            fn.setValueName(v, pname);
                            fn.block(blockId).params().push_back(v);
                            valueByName[pname] = v;
                            if (at(Tok::Comma))
                                advance();
                            else
                                break;
                        }
                        expect(Tok::RParen, "')'");
                    }
                    expect(Tok::Colon, "':'");

                    // Instructions, then exactly one terminator.
                    bool sawTerminator = false;
                    while (!sawTerminator) {
                        if (at(Tok::Percent)) {
                            // %name = <instruction>
                            std::string resultName = expectPercent();
                            expect(Tok::Equals, "'='");
                            Instruction inst = parseInstructionBody(m, fn, resolveValue);
                            ValueId result = fn.newValue(inst.type);
                            inst.result = result;
                            fn.setValueName(result, resultName);
                            fn.addInst(blockId, inst);
                            valueByName[resultName] = result;
                        } else if (atIdent("store")) {
                            Instruction inst = parseStore(m, fn, resolveValue);
                            fn.addInst(blockId, inst);
                        } else if (atIdent("call")) {
                            advance();
                            Instruction inst = parseCall(m, fn, resolveValue, /*hasResult=*/false);
                            fn.addInst(blockId, inst);
                        } else if (atIdent("br") || atIdent("condbr") || atIdent("ret") || atIdent("switch") ||
                                   atIdent("unreachable")) {
                            fn.block(blockId).term() = parseTerminator(m, fn, resolveValue, resolveBlockRef);
                            sawTerminator = true;
                        } else {
                            throw ParseError("expected an instruction or terminator, got '" + cur().text + "'");
                        }
                    }
                }
            }

            using ResolveValueFn = std::function<ValueId()>;
            using ResolveBlockRefFn = std::function<BlockRef()>;

            Instruction parseStore(Module &m, Function &, const ResolveValueFn &resolveValue) {
                expectIdent("store");
                parseType(m);  // redundant with the value's own recorded type; consumed, not stored
                Instruction inst;
                inst.op = Opcode::Store;
                inst.type = m.types().voidType();
                ValueId value = resolveValue();
                expect(Tok::Comma, "','");
                ValueId ptr = resolveValue();
                inst.operands = {value, ptr};
                return inst;
            }

            // Assumes the "call" keyword itself has already been consumed
            // by the caller -- the two call sites are at different points
            // relative to it (the void-call site sees it first; the
            // with-result site already consumed it as the instruction's
            // opcode token before dispatching here).
            Instruction parseCall(Module &m, Function &, const ResolveValueFn &resolveValue, bool hasResult) {
                Instruction inst;
                inst.op = Opcode::Call;
                if (hasResult) {
                    inst.type = parseType(m);
                } else {
                    expectIdent("void");
                    inst.type = m.types().voidType();
                }
                std::string calleeName = expectAt();
                FuncId callee{};
                for (std::size_t i = 0; i < m.functions().size(); ++i) {
                    if (m.functions()[i].name() == calleeName) {
                        callee = FuncId(static_cast<FuncId::Value>(i));
                        break;
                    }
                }
                if (!callee.isValid())
                    throw ParseError("call to undeclared function '@" + calleeName + "'");
                inst.callee = callee;
                expect(Tok::LParen, "'('");
                while (!at(Tok::RParen)) {
                    inst.operands.push_back(resolveValue());
                    if (at(Tok::Comma))
                        advance();
                    else
                        break;
                }
                expect(Tok::RParen, "')'");
                return inst;
            }

            Instruction parseInstructionBody(Module &m, Function &fn, const ResolveValueFn &resolveValue) {
                std::string op = expectAnyIdent();

                if (op == "const") {
                    TypeId ty = parseType(m);
                    Instruction inst;
                    inst.op = Opcode::Const;
                    inst.type = ty;
                    std::string lit = expectNumber();
                    if (m.types().get(ty).kind == TypeKind::Float) {
                        double d = std::stod(lit);
                        std::uint64_t bits;
                        if (m.types().get(ty).bits == 32) {
                            float f = static_cast<float>(d);
                            std::uint32_t b32;
                            std::memcpy(&b32, &f, sizeof(f));
                            bits = b32;
                        } else {
                            std::memcpy(&bits, &d, sizeof(d));
                        }
                        inst.constant.bits = bits;
                    } else if (m.types().get(ty).isSigned) {
                        inst.constant.bits = static_cast<std::uint64_t>(std::stoll(lit));
                    } else {
                        inst.constant.bits = static_cast<std::uint64_t>(std::stoull(lit));
                    }
                    return inst;
                }
                if (op == "alloca") {
                    Instruction inst;
                    inst.op = Opcode::Alloca;
                    inst.elemType = parseType(m);
                    inst.type = m.types().ptrType(inst.elemType);
                    if (at(Tok::Comma)) {
                        advance();
                        expectIdent("align");
                        inst.align = static_cast<std::uint32_t>(std::stoul(expectNumber()));
                    }
                    return inst;
                }
                if (op == "load") {
                    Instruction inst;
                    inst.op = Opcode::Load;
                    inst.type = parseType(m);
                    expect(Tok::Comma, "','");
                    inst.operands = {resolveValue()};
                    return inst;
                }
                if (auto it = kBinopKeywords.find(op); it != kBinopKeywords.end()) {
                    Instruction inst;
                    inst.op = it->second;
                    inst.type = parseType(m);
                    ValueId a = resolveValue();
                    expect(Tok::Comma, "','");
                    ValueId b = resolveValue();
                    inst.operands = {a, b};
                    return inst;
                }
                if (op == "icmp" || op == "fcmp") {
                    Instruction inst;
                    inst.op = op == "icmp" ? Opcode::ICmp : Opcode::FCmp;
                    std::string predTok = expectAnyIdent();
                    const auto &table = op == "icmp" ? kIcmpPreds : kFcmpPreds;
                    auto pit = table.find(predTok);
                    if (pit == table.end())
                        throw ParseError("unknown comparison predicate '" + predTok + "'");
                    inst.pred = pit->second;
                    parseType(m);  // operand type, redundant with the operands' own recorded types
                    ValueId a = resolveValue();
                    expect(Tok::Comma, "','");
                    ValueId b = resolveValue();
                    inst.operands = {a, b};
                    inst.type = m.types().boolType();
                    return inst;
                }
                if (auto it = kUnopKeywords.find(op); it != kUnopKeywords.end()) {
                    Instruction inst;
                    inst.op = it->second;
                    inst.type = parseType(m);
                    inst.operands = {resolveValue()};
                    return inst;
                }
                if (auto it = kCastKeywords.find(op); it != kCastKeywords.end()) {
                    Instruction inst;
                    inst.op = it->second;
                    ValueId v = resolveValue();
                    expectIdent("to");
                    inst.type = parseType(m);
                    inst.operands = {v};
                    return inst;
                }
                if (op == "gep") {
                    Instruction inst;
                    inst.op = Opcode::Gep;
                    inst.elemType = parseType(m);
                    inst.type = m.types().ptrType(inst.elemType);
                    expect(Tok::Comma, "','");
                    inst.operands.push_back(resolveValue());
                    while (at(Tok::Comma)) {
                        advance();
                        inst.operands.push_back(resolveValue());
                    }
                    return inst;
                }
                if (op == "call") {
                    return parseCall(m, fn, resolveValue, /*hasResult=*/true);
                }
                if (op == "select") {
                    Instruction inst;
                    inst.op = Opcode::Select;
                    ValueId cond = resolveValue();
                    expect(Tok::Comma, "','");
                    ValueId a = resolveValue();
                    expect(Tok::Comma, "','");
                    ValueId b = resolveValue();
                    inst.operands = {cond, a, b};
                    inst.type = fn.typeOf(a);
                    return inst;
                }
                throw ParseError("unknown instruction opcode '" + op + "'");
            }

            Terminator parseTerminator(Module &m, Function &fn, const ResolveValueFn &resolveValue,
                                       const ResolveBlockRefFn &resolveBlockRef) {
                Terminator t;
                if (atIdent("br")) {
                    advance();
                    t.kind = TermKind::Br;
                    t.targets = {resolveBlockRef()};
                } else if (atIdent("condbr")) {
                    advance();
                    t.kind = TermKind::CondBr;
                    t.cond = resolveValue();
                    expect(Tok::Comma, "','");
                    t.targets.push_back(resolveBlockRef());
                    expect(Tok::Comma, "','");
                    t.targets.push_back(resolveBlockRef());
                } else if (atIdent("ret")) {
                    advance();
                    t.kind = TermKind::Ret;
                    if (atIdent("void")) {
                        advance();
                    } else {
                        parseType(m);  // redundant with the value's own recorded type
                        t.retValue = resolveValue();
                    }
                } else if (atIdent("switch")) {
                    advance();
                    t.kind = TermKind::Switch;
                    parseType(m);
                    t.cond = resolveValue();
                    expect(Tok::Comma, "','");
                    expectIdent("default");
                    t.targets.push_back(resolveBlockRef());
                    expect(Tok::LBracket, "'['");
                    while (!at(Tok::RBracket)) {
                        std::string numTok = expectNumber();
                        t.caseValues.push_back(std::stoll(numTok));
                        t.targets.push_back(resolveBlockRef());
                        if (at(Tok::Comma))
                            advance();
                    }
                    expect(Tok::RBracket, "']'");
                } else if (atIdent("unreachable")) {
                    advance();
                    t.kind = TermKind::Unreachable;
                } else {
                    throw ParseError("expected a terminator, got '" + cur().text + "'");
                }
                (void)fn;
                return t;
            }
        };

    }  // namespace

    std::optional<Module> TextParser::parse(std::string_view text, std::string &error) {
        try {
            ParserImpl impl(text);
            return impl.parseModule();
        } catch (const std::exception &e) {
            error = e.what();
            return std::nullopt;
        }
    }

}  // namespace zust::zir
