#include "all.h"

ir_node_t *ir_memdup(ir_node_t node) {
	ir_node_t *ptr = malloc(sizeof(ir_node_t));
	*ptr = node;
	return ptr;
}
