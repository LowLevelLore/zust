#include <doctest/doctest.h>

#include "zir/Builder.hpp"
#include "zir/Printer.hpp"

using namespace zust::zir;

namespace {
    // Hand-builds exactly the module docs/IR-DESIGN.md's "Textual form"
    // section shows, so Printer::print can be compared against that text
    // byte-for-byte (docs/PRD-ZIR.md Wave 1.3's exit criterion).
    Module buildFactorialExample() {
        Module m("hello.zz", "generic");
        TypeTable &types = m.types();

        TypeId i8 = types.intType(8, true);
        TypeId i32 = types.intType(32, true);
        TypeId i64 = types.intType(64, true);
        TypeId ptr = types.ptrType(i8);

        // @.str0 = private constant [4 x i8] c"%d\0A\00"
        GlobalVar str0;
        str0.name = ".str0";
        str0.type = types.arrayType(i8, 4);
        str0.isPrivate = true;
        str0.isConstant = true;
        str0.hasInit = true;
        str0.initBytes = std::string("%d\n") + '\0';  // '%','d','\n'(0x0A),'\0'(0x00)
        m.addGlobal(str0);

        // declare i32 @printf(ptr, ...) variadic
        TypeId printfSig = types.fnType({ptr}, i32, true);
        m.addFunction(Function("printf", printfSig, /*isExtern=*/true, /*isVariadic=*/true));

        // fn @factorial(%n: i64) -> i64 { ... }
        TypeId factorialSig = types.fnType({i64}, i64, false);
        FuncId factorialId = m.addFunction(Function("factorial", factorialSig, false, false));
        Function &fn = m.function(factorialId);
        Builder b(m, fn);

        BlockId entry = b.createBlock("entry");
        BlockId base = b.createBlock("base");
        BlockId rec = b.createBlock("rec");
        fn.setEntry(entry);

        ValueId n = b.addBlockParam(entry, i64);
        fn.setValueName(n, "n");

        b.setInsertBlock(entry);
        ValueId c1 = b.constInt(i64, 1);
        fn.setValueName(c1, "c1");
        ValueId cmp = b.icmp(CmpPred::Sle, types.boolType(), n, c1);
        fn.setValueName(cmp, "cmp");
        b.condBr(cmp, base, {}, rec, {});

        b.setInsertBlock(base);
        b.ret(c1);

        b.setInsertBlock(rec);
        ValueId sub = b.binop(Opcode::Sub, i64, n, c1);
        fn.setValueName(sub, "sub");
        ValueId rc = b.call(factorialId, i64, {sub});
        fn.setValueName(rc, "rc");
        ValueId mul = b.binop(Opcode::Mul, i64, n, rc);
        fn.setValueName(mul, "mul");
        b.ret(mul);

        return m;
    }
}  // namespace

TEST_CASE("Printer reproduces docs/IR-DESIGN.md's @factorial example byte-for-byte") {
    Module m = buildFactorialExample();
    std::string expected =
        "; hello.zz\n"
        "module \"hello.zz\" target = \"generic\"\n"
        "\n"
        "@.str0 = private constant [4 x i8] c\"%d\\0A\\00\"\n"
        "\n"
        "declare i32 @printf(ptr, ...) variadic\n"
        "\n"
        "fn @factorial(%n: i64) -> i64 {\n"
        "^entry(%n: i64):\n"
        "    %c1   = const i64 1\n"
        "    %cmp  = icmp sle i64 %n, %c1\n"
        "    condbr %cmp, ^base, ^rec\n"
        "\n"
        "^base:\n"
        "    ret i64 %c1\n"
        "\n"
        "^rec:\n"
        "    %sub  = sub i64 %n, %c1\n"
        "    %rc   = call i64 @factorial(%sub)\n"
        "    %mul  = mul i64 %n, %rc\n"
        "    ret i64 %mul\n"
        "}\n";

    std::string actual = Printer::print(m);
    CHECK(actual == expected);
}
