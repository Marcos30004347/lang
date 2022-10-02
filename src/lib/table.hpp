#pragma once

#include "types.hpp"
#include "utils.hpp"

namespace lib {

template < typename K, typename T > struct TableNode {
  K          key;
  T          val;
  TableNode* left;
  TableNode* right;
  i64        height;
  u64        size;
};

template < typename K, typename T > void table_delete(TableNode< K, T >* t, void (*destroy_value)(T) = 0) {
  if (t == 0)
    return;

  table_delete(t->left);
  table_delete(t->right);

  if (destroy_value) {
    destroy_value(t->val);
  }

  delete t;
}

template < typename K, typename T > i64 height(TableNode< K, T >* N) {
  if (N == 0) {
    return 0;
  }

  return N->height;
}

template < typename K, typename T > TableNode< K, T >* create_table_node(K key, T val) {
  TableNode< K, T >* node = new TableNode< K, T >();
  node->key               = key;
  node->left              = 0;
  node->right             = 0;
  node->height            = 1;
  node->val               = val;
  node->size              = 1;
  return node;
}

template < typename K, typename T > void update_size(TableNode< K, T >* x) {
  x->size = 1;

  if (x->left) {
    x->size += x->left->size;
  }

  if (x->right) {
    x->size += x->right->size;
  }
}

template < typename K, typename T > TableNode< K, T >* right_rotate(TableNode< K, T >* y) {
  TableNode< K, T >* x  = y->left;
  TableNode< K, T >* T2 = x->right;

  x->right = y;
  y->left  = T2;

  y->height = max(height(y->left), height(y->right)) + 1;
  x->height = max(height(x->left), height(x->right)) + 1;

  update_size(x);
  update_size(y);

  return x;
}

template < typename K, typename T > TableNode< K, T >* left_rotate(TableNode< K, T >* x) {
  TableNode< K, T >* y  = x->right;
  TableNode< K, T >* T2 = y->left;

  y->left  = x;
  x->right = T2;

  x->height = max(height(x->left), height(x->right)) + 1;
  y->height = max(height(y->left), height(y->right)) + 1;

  update_size(x);
  update_size(y);

  return y;
}

template < typename K, typename T > i64 getBalance(TableNode< K, T >* N) {
  if (N == 0) {
    return 0;
  }
  return height(N->left) - height(N->right);
}

template < typename K, typename T > TableNode< K, T >* insert_node(TableNode< K, T >* node, K key, T crc) {
  if (node == 0) {
    return (create_table_node(key, crc));
  }

  if (is_smaller(key, node->key)) {
    node->left = insert_node(node->left, key, crc);
  } else if (is_greater(key, node->key)) {
    node->right = insert_node(node->right, key, crc);
  } else {
    update_size(node);
    return node;
  }

  node->height = 1 + max(height(node->left), height(node->right));

  i64 balance = getBalance(node);

  if (balance > 1 && is_smaller(key, node->left->key)) {
    return right_rotate(node);
  }
  if (balance < -1 && is_greater(key, node->right->key)) {
    return left_rotate(node);
  }

  if (balance > 1 && is_greater(key, node->left->key)) {
    node->left = left_rotate(node->left);
    return right_rotate(node);
  }

  if (balance < -1 && is_smaller(key, node->right->key)) {
    node->right = right_rotate(node->right);
    return left_rotate(node);
  }

  update_size(node);

  return node;
}

template < typename K, typename T > T* search(struct TableNode< K, T >* root, K key) {
  if (root == 0) {
    return 0;
  }

  if (is_equal(root->key, key)) {
    return &root->val;
  }

  if (is_smaller(root->key, key)) {
    return search(root->right, key);
  }

  return search(root->left, key);
}

template < typename K, typename T > TableNode< K, T >* min_value_node(TableNode< K, T >* node) {
  TableNode< K, T >* current = node;

  while (current->left != 0) {
    current = current->left;
  }

  return current;
}

template < typename K, typename T > TableNode< K, T >* remove(TableNode< K, T >* root, K key) {

  if (root == 0) {
    return root;
  }

  if (is_smaller(key, root->key)) {
    root->left = remove< T >(root->left, key);

  } else if (is_greater(key, root->key)) {
    root->right = remove< T >(root->right, key);
  } else {
    if ((root->left == 0) || (root->right == 0)) {
      TableNode< K, T >* temp = root->left ? root->left : root->right;

      if (temp == 0) {
        temp = root;
        root = 0;
      } else
        *root = *temp;

      free(temp);
    } else {
      TableNode< K, T >* temp = min_value_node< T >(root->right);

      root->key = temp->key;

      root->right = remove< T >(root->right, temp->key);
    }
  }

  if (root == 0)
    return root;

  root->height = 1 + max(height(root->left), height(root->right));

  i64 balance = getBalance(root);

  if (balance > 1 && getBalance(root->left) >= 0)
    return rightRotate(root);

  if (balance > 1 && getBalance(root->left) < 0) {
    root->left = leftRotate(root->left);
    return rightRotate(root);
  }

  if (balance < -1 && getBalance(root->right) <= 0)
    return leftRotate(root);

  if (balance < -1 && getBalance(root->right) > 0) {
    root->right = rightRotate(root->right);
    return leftRotate(root);
  }

  update_size(root);

  return root;
}

template < typename K, typename T > u64 size(TableNode< K, T >* node) {
  if (node == 0) {
    return 0;
  }

  return node->size;
}

template < typename K, typename T > struct KeyValuePair {
  K key;
  T val;
};

template < typename K, typename T > KeyValuePair< K, T* >* get_ith(TableNode< K, T >* node, u64 index) {
  if (node == 0)
    return 0;

  if (!node->left && index == 0) {
    KeyValuePair< K, T* > pair;

    pair.key = node->key;
    pair.val = &node->val;

    return pair;
  }

  if (node->left && node->left->size == index) {
    KeyValuePair< K, T* > pair;

    pair.key = node->key;
    pair.val = &node->val;

    return pair;
  }

  if (node->left && index > node->left->size) {
    return get_ith(node->right, index - node->left->size - 1);
  }

  return get_ith(node->left, index - 1);
}

template < typename K, typename T > TableNode< K, T >* copy(TableNode< K, T >* node) {
  TableNode< K, T >* c = new TableNode< K, T >();

  c->left  = copy(node->left);
  c->right = copy(node->right);

  c->size   = node->size;
  c->key    = node->key;
  c->height = node->height;
  c->val    = node->val;

  return c;
}

template < typename K, typename T > struct Table { TableNode< K, T >* root; };

template < typename K, typename T > void table_delete(Table< K, T >* t, void (*destroy_value)(T) = 0) {
  if (t->root) {
    table_delete(t->root, destroy_value);
  }
  delete t;
}

template < typename K, typename T > Table< K, T >* table_create() {
  Table< K, T >* h = new Table< K, T >();
  h->root          = 0;
  return h;
}

template < typename K, typename T > Table< K, T >* copy(Table< K, T >* t) {
  Table< K, T >* h = new Table< K, T >();
  h->root          = copy(t);
  return h;
}

template < typename K, typename T > void insert(Table< K, T >* node, K key, T val) {
  node->root = insert_node(node->root, key, val);
}

template < typename K, typename T > void remove(Table< K, T >* node, K key) {
  node->root = remove(node->root, key);
}

template < typename K, typename T > T* search(Table< K, T >* node, K key) {
  return search(node->root, key);
}

template < typename K, typename T > u64 size(Table< K, T >* node) {
  return size(node->root);
}

} // namespace lib
