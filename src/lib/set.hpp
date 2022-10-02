#pragma once

#include "types.hpp"
#include "utils.hpp"

namespace lib {

template < typename T > struct SetNode {
  T        key;
  SetNode* left;
  SetNode* right;
  i64      height;
  u64      size;
};

template < typename T > void setnode_delete(SetNode< T >* t, void (*destroy_key)(T) = 0) {
  if (t == 0)
    return;

  setnode_delete(t->left);
  setnode_delete(t->right);

  if (destroy_key) {
    destroy_key(t->key);
  }

  delete t;
}

template < typename T > i64 height(SetNode< T >* N) {
  if (N == 0) {
    return 0;
  }

  return N->height;
}

template < typename T > SetNode< T >* create_set_node(T key) {
  SetNode< T >* node = new SetNode< T >();
  node->key          = key;
  node->left         = 0;
  node->right        = 0;
  node->height       = 1;
  node->size         = 1;
  return node;
}

template < typename T > void update_size(SetNode< T >* x) {
  x->size = 1;

  if (x->left) {
    x->size += x->left->size;
  }
  if (x->right) {
    x->size += x->right->size;
  }
}
template < typename T > SetNode< T >* right_rotate(SetNode< T >* y) {
  SetNode< T >* x = y->left;
  SetNode< T >* t = x->right;

  x->right = y;
  y->left  = t;

  y->height = max(height(y->left), height(y->right)) + 1;
  x->height = max(height(x->left), height(x->right)) + 1;

  update_size(x);
  update_size(y);

  return x;
}

template < typename T > SetNode< T >* left_rotate(SetNode< T >* x) {
  SetNode< T >* y = x->right;
  SetNode< T >* t = y->left;

  y->left  = x;
  x->right = t;

  x->height = max(height(x->left), height(x->right)) + 1;
  y->height = max(height(y->left), height(y->right)) + 1;

  update_size(x);
  update_size(y);

  return y;
}

template < typename T > i64 getBalance(SetNode< T >* N) {
  if (N == 0) {
    return 0;
  }

  return height(N->left) - height(N->right);
}

template < typename T > SetNode< T >* insert_node(SetNode< T >* node, T key) {
  if (node == 0) {
    return (create_set_node(key));
  }

  if (is_smaller(key, node->key)) {
    node->left = insert_node(node->left, key);
  } else if (is_greater(key, node->key)) {
    node->right = insert_node(node->right, key);
  } else {
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

template < typename T > T* search(struct SetNode< T >* root, T key) {
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

template < typename T > SetNode< T >* min_value_node(SetNode< T >* node) {
  SetNode< T >* current = node;

  while (current->left != 0) {
    current = current->left;
  }

  return current;
}

template < typename T > SetNode< T >* remove(SetNode< T >* root, T key) {

  if (root == 0) {
    return root;
  }

  if (is_smaller(key, root->key)) {
    root->left = remove< T >(root->left, key);

  } else if (is_greater(key, root->key)) {
    root->right = remove< T >(root->right, key);
  } else {
    if ((root->left == 0) || (root->right == 0)) {
      SetNode< T >* temp = root->left ? root->left : root->right;

      if (temp == 0) {
        temp = root;
        root = 0;
      } else
        *root = *temp;

      free(temp);
    } else {
      SetNode< T >* temp = min_value_node< T >(root->right);

      root->key = temp->key;

      root->right = remove< T >(root->right, temp->key);
    }
  }

  if (root == 0) {
    return root;
  }

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

template < typename T > u64 size(SetNode< T >* node) {
  if (node == 0) {
    return 0;
  }
  return node->size;
}

template < typename T > T* get_ith(SetNode< T >* node, u64 index) {
  if (node == 0)
    return 0;

  if (!node->left && index == 0) {
    return &node->key;
  }

  if (node->left && node->left->size == index) {
    return &node->key;
  }

  if (node->left && index > node->left->size) {
    return get_ith(node->right, index - node->left->size - 1);
  }

  return get_ith(node->left, index - 1);
}

template < typename T > SetNode< T >* copy(SetNode< T >* node) {
  SetNode< T >* c = new SetNode< T >();

  c->left  = copy(node->left);
  c->right = copy(node->right);

  c->size   = node->size;
  c->key    = node->key;
  c->height = node->height;

  return c;
}
template < typename T > struct Set { SetNode< T >* root; };

template < typename T > void set_delete(Set< T >* t, void (*destroy_value)(T) = 0) {
  if (t->root) {
    setnode_delete(t->root, destroy_value);
  }
  delete t;
}

template < typename T > Set< T >* set_create() {
  Set< T >* h = new Set< T >();
  h->root     = 0;
  return h;
}

template < typename T > Set< T >* copy(Set< T >* t) {
  Set< T >* h = new Set< T >();
  h->root     = copy(t);
  return h;
}

template < typename T > void insert(Set< T >* node, T val) {
  node->root = insert_node(node->root, val);
}

template < typename T > void remove(Set< T >* node, T key) {
  node->root = remove(node->root, key);
}

template < typename T > T* search(Set< T >* node, T key) {
  return search(node->root, key);
}

template < typename T > u64 size(Set< T >* node) {
  return size(node->root);
}
} // namespace lib
