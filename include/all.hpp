#pragma once
#include "common/Colors.hpp"
#include "common/Errors.hpp"
#include "common/Logging.hpp"
#include "common/StringUtils.hpp"

#include "support/CommandLine.hpp"
#include "support/File.hpp"

#include "ast/ASTNode.hpp"

#include "parser/Parser.hpp"
#include "parser/ScopeContext.hpp"

#include "lexer/Lexer.hpp"

#include "typechecker/TypeChecker.hpp"

#include "codegen/CodeGen.hpp"
#include "codegen/RegisterAllocator.hpp"

#include <sstream>
#include <map>
#include <iostream>
#include <memory>
#include <fstream>
#include <assert.h>
#include <iomanip>