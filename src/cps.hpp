#pragma once

/*
	Continuation Passing Style Analysis
*/

#include "ast.hpp"
#include "context.hpp"

struct FreeVars {
	Scope* scope;
	
	AST_Id func_decl_id;
 
	FreeVars* next;
};


struct CPS_Analysis {
	FreeVars* free_vars;
	
};
