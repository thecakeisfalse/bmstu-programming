#undef _FORTIFY_SOURCE

#include <cstdio>

#include <gcc-plugin.h>
#include <coretypes.h>
#include <context.h>
#include <input.h>
#include <basic-block.h>
#include <tree-core.h>
#include <tree.h>
#include <real.h>
#include <function.h>
#include <tree-pass.h>
#include <gimple.h>
#include <gimple-iterator.h>
#include <gimple-ssa.h>

int plugin_is_GPL_compatible;

struct GimpleViewer {
    FILE* out;
    function* fun;

    void print_function() {
        fprintf(
            out, "fn %s  [%s:%d]\n", function_name(fun), LOCATION_FILE(fun->function_start_locus),
            LOCATION_LINE(fun->function_start_locus)
        );

        basic_block bb;
        FOR_ALL_BB_FN(bb, fun) {
            print_bb(bb);
        }

        fprintf(out, "\n");
    }

    void print_bb_ref(int idx) {
        if (idx == ENTRY_BLOCK) {
            fprintf(out, "ENTRY");
        } else if (idx == EXIT_BLOCK) {
            fprintf(out, "EXIT");
        } else {
            fprintf(out, "bb%d", idx);
        }
    }

    void print_bb(basic_block bb) {
        fprintf(out, "  ");
        print_bb_ref(bb->index);

        fprintf(out, "  preds:[");
        {
            edge e;
            edge_iterator ei;
            bool first = true;
            FOR_EACH_EDGE(e, ei, bb->preds) {
                if (!first) {
                    fputc(' ', out);
                }
                print_bb_ref(e->src->index);
                first = false;
            }
        }
        fprintf(out, "]  succs:[");
        {
            edge e;
            edge_iterator ei;
            bool first = true;
            FOR_EACH_EDGE(e, ei, bb->succs) {
                if (!first) {
                    fputc(' ', out);
                }
                print_bb_ref(e->dest->index);
                first = false;
            }
        }
        fprintf(out, "]\n");

        for (gphi_iterator pi = gsi_start_phis(bb); !gsi_end_p(pi); gsi_next(&pi)) {
            print_phi(pi.phi());
        }

        for (gimple_stmt_iterator si = gsi_start_bb(bb); !gsi_end_p(si); gsi_next(&si)) {
            print_stmt(gsi_stmt(si));
        }
    }

    void print_phi(gphi* phi) {
        fprintf(out, "    phi    ");
        print_tree(gimple_phi_result(phi));
        fprintf(out, " = PHI(");

        for (unsigned i = 0; i < gimple_phi_num_args(phi); i++) {
            if (i > 0) {
                fprintf(out, ", ");
            }

            print_tree(gimple_phi_arg(phi, i)->def);
            fprintf(out, "@");
            print_bb_ref(gimple_phi_arg_edge(phi, i)->src->index);
        }

        fprintf(out, ")\n");
    }

    void print_stmt(gimple* stmt) {
        switch (gimple_code(stmt)) {
            case GIMPLE_ASSIGN:
                print_assign(as_a<gassign*>(stmt));
                break;
            case GIMPLE_CALL:
                print_call(as_a<gcall*>(stmt));
                break;
            case GIMPLE_COND:
                print_cond(as_a<gcond*>(stmt));
                break;
            case GIMPLE_RETURN:
                print_return(as_a<greturn*>(stmt));
                break;
            case GIMPLE_LABEL:
            case GIMPLE_PREDICT:
                break;
            default:
                fprintf(out, "    <gimple:%d>\n", (int) gimple_code(stmt));
        }
    }

    void print_assign(gassign* stmt) {
        fprintf(out, "    assign ");
        print_tree(gimple_assign_lhs(stmt));
        fprintf(out, " = ");

        tree_code rhs_code = gimple_assign_rhs_code(stmt);

        switch (gimple_assign_rhs_class(stmt)) {
            case GIMPLE_SINGLE_RHS:
                print_tree(gimple_assign_rhs1(stmt));
                break;

            case GIMPLE_UNARY_RHS:
                print_op(rhs_code);
                print_tree(gimple_assign_rhs1(stmt));
                break;

            case GIMPLE_BINARY_RHS:
                print_tree(gimple_assign_rhs1(stmt));
                fprintf(out, " ");
                print_op(rhs_code);
                fprintf(out, " ");
                print_tree(gimple_assign_rhs2(stmt));
                break;

            default:
                break;
        }

        fprintf(out, "\n");
    }

    void print_call(gcall* stmt) {
        fprintf(out, "    call   ");
        tree lhs = gimple_call_lhs(stmt);

        if (lhs) {
            print_tree(lhs);
            fprintf(out, " = ");
        }

        tree fndecl = gimple_call_fndecl(stmt);

        if (fndecl) {
            fprintf(out, "%s", fndecl_name(fndecl));
        } else {
            print_tree(gimple_call_fn(stmt));
        }

        fprintf(out, "(");

        for (unsigned i = 0; i < gimple_call_num_args(stmt); i++) {
            if (i > 0) {
                fprintf(out, ", ");
            }

            print_tree(gimple_call_arg(stmt, i));
        }

        fprintf(out, ")\n");
    }

    void print_cond(gcond* stmt) {
        fprintf(out, "    cond   ");
        print_tree(gimple_cond_lhs(stmt));

        fprintf(out, " ");
        print_op(gimple_cond_code(stmt));

        fprintf(out, " ");
        print_tree(gimple_cond_rhs(stmt));

        edge e;
        edge_iterator ei;

        FOR_EACH_EDGE(e, ei, gimple_bb(stmt)->succs) {
            if (e->flags & EDGE_TRUE_VALUE) {
                fprintf(out, "  ->T:");
                print_bb_ref(e->dest->index);
            } else if (e->flags & EDGE_FALSE_VALUE) {
                fprintf(out, "  ->F:");
                print_bb_ref(e->dest->index);
            }
        }

        fprintf(out, "\n");
    }

    void print_return(greturn* stmt) {
        fprintf(out, "    return ");
        tree val = gimple_return_retval(stmt);

        if (val) {
            print_tree(val);
        } else {
            fprintf(out, "void");
        }

        fprintf(out, "\n");
    }

    void print_tree(tree t) {
        if (!t) {
            fprintf(out, "<null>");
            return;
        }

        switch (TREE_CODE(t)) {
            case INTEGER_CST:
                fprintf(out, "%ld", (long) TREE_INT_CST_LOW(t));
                break;

            case SSA_NAME: {
                tree id = SSA_NAME_IDENTIFIER(t);
                fprintf(out, "%s_v%u", id ? IDENTIFIER_POINTER(id) : "tmp", SSA_NAME_VERSION(t));
                break;
            }

            case VAR_DECL:
            case PARM_DECL: {
                tree name = DECL_NAME(t);
                fprintf(out, "%s", name ? IDENTIFIER_POINTER(name) : "<tmp>");
                break;
            }

            case RESULT_DECL:
                break;

            case ARRAY_REF:
                print_tree(TREE_OPERAND(t, 0));
                fprintf(out, "[");
                print_tree(TREE_OPERAND(t, 1));
                fprintf(out, "]");
                break;

            case MEM_REF: {
                fprintf(out, "*(");
                print_tree(TREE_OPERAND(t, 0));
                tree off = TREE_OPERAND(t, 1);
                if (TREE_INT_CST_LOW(off) != 0) {
                    fprintf(out, " + %ld", (long) TREE_INT_CST_LOW(off));
                }
                fprintf(out, ")");
                break;
            }

            case COMPONENT_REF:
                print_tree(TREE_OPERAND(t, 0));
                fprintf(out, ".");
                print_tree(TREE_OPERAND(t, 1));
                break;

            case FIELD_DECL: {
                tree name = DECL_NAME(t);
                fprintf(out, "%s", name ? IDENTIFIER_POINTER(name) : "<field>");
                break;
            }

            case ADDR_EXPR:
                fprintf(out, "&");
                print_tree(TREE_OPERAND(t, 0));
                break;

            case STRING_CST:
                fprintf(out, "\"%s\"", TREE_STRING_POINTER(t));
                break;

            case CONSTRUCTOR:
                break;

            default:
                fprintf(out, "<tree:%d>", (int) TREE_CODE(t));
                break;
        }
    }

    void print_op(tree_code code) {
        switch (code) {
            case PLUS_EXPR:
                fprintf(out, "+");
                break;
            case MINUS_EXPR:
                fprintf(out, "-");
                break;
            case MULT_EXPR:
                fprintf(out, "*");
                break;
            case RDIV_EXPR:
            case TRUNC_DIV_EXPR:
                fprintf(out, "/");
                break;
            case TRUNC_MOD_EXPR:
                fprintf(out, "%%");
                break;
            case BIT_AND_EXPR:
                fprintf(out, "&");
                break;
            case BIT_IOR_EXPR:
                fprintf(out, "|");
                break;
            case BIT_XOR_EXPR:
                fprintf(out, "^");
                break;
            case BIT_NOT_EXPR:
                fprintf(out, "~");
                break;
            case LSHIFT_EXPR:
                fprintf(out, "<<");
                break;
            case RSHIFT_EXPR:
                fprintf(out, ">>");
                break;
            case NEGATE_EXPR:
                fprintf(out, "-");
                break;
            case LT_EXPR:
                fprintf(out, "<");
                break;
            case LE_EXPR:
                fprintf(out, "<=");
                break;
            case GT_EXPR:
                fprintf(out, ">");
                break;
            case GE_EXPR:
                fprintf(out, ">=");
                break;
            case EQ_EXPR:
                fprintf(out, "==");
                break;
            case NE_EXPR:
                fprintf(out, "!=");
                break;
            case POINTER_PLUS_EXPR:
                fprintf(out, "+");
                break;
            case NOP_EXPR:
                fprintf(out, "(cast)");
                break;
            case FLOAT_EXPR:
                fprintf(out, "(float)");
                break;
            default:
                fprintf(out, "<op:%d>", (int) code);
                break;
        }
    }
};

static const pass_data gimple_pass_data = {
    .type = GIMPLE_PASS,
    .name = "gimpler",
    .optinfo_flags = OPTGROUP_NONE,
    .tv_id = TV_NONE,
    .properties_required = PROP_ssa,
    .properties_provided = 0,
    .properties_destroyed = 0,
    .todo_flags_start = 0,
    .todo_flags_finish = 0,
};

struct GimpleViewPass : gimple_opt_pass {
    GimpleViewPass(gcc::context* ctx) : gimple_opt_pass(gimple_pass_data, ctx) {}

    unsigned int execute(function* fun) override {
        GimpleViewer { stderr, fun }.print_function();
        return 0;
    }

    GimpleViewPass* clone() override { return this; }
};

int plugin_init(struct plugin_name_args* args, struct plugin_gcc_version*) {
    static register_pass_info pass_info = {
        .pass = new GimpleViewPass(g),
        .reference_pass_name = "ssa",
        .ref_pass_instance_number = 1,
        .pos_op = PASS_POS_INSERT_AFTER,
    };
    register_callback(args->base_name, PLUGIN_PASS_MANAGER_SETUP, nullptr, &pass_info);
    return 0;
}
