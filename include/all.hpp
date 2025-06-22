#pragma once

#include <assert.h>

#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>

#include "ast/ASTNode.hpp"
#include "codegen/Canaries.hpp"
#include "codegen/CodeGen.hpp"
#include "codegen/RegisterAllocator.hpp"
#include "common/Colors.hpp"
#include "common/Errors.hpp"
#include "common/Logging.hpp"
#include "common/StringUtils.hpp"
#include "lexer/Lexer.hpp"
#include "parser/NameMapper.hpp"
#include "parser/Parser.hpp"
#include "parser/ScopeContext.hpp"
#include "support/CommandLine.hpp"
#include "support/File.hpp"
#include "typechecker/TypeChecker.hpp"

static zust::NameMapper GLOBAL_NAME_MAPPER;