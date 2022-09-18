#pragma once

#include "types.hpp"
namespace lib {

template < typename T > struct Table {
  u64    key;
  T      val;
  Table* left;
  Table* right;
  i64    height;
};

template < typename T > T max(T a, T b) {
  return (a > b) ? a : b;
}

template < typename T > i64 height(Table< T >* N) {
  if (N == 0) {
    return 0;
  }

  return N->height;
}

template < typename T > Table< T >* create_id_to_crc64_map_node(u64 key, T val) {
  Table< T >* node = new Table< T >();
  node->key        = key;
  node->left       = 0;
  node->right      = 0;
  node->height     = 1;
  node->val        = val;

  return node;
}

template < typename T > Table< T >* right_rotate(Table< T >* y) {
  Table< T >* x  = y->left;
  Table< T >* T2 = x->right;

  x->right = y;
  y->left  = T2;

  y->height = max(height(y->left), height(y->right)) + 1;
  x->height = max(height(x->left), height(x->right)) + 1;

  return x;
}

template < typename T > Table< T >* left_rotate(Table< T >* x) {
  Table< T >* y  = x->right;
  Table< T >* T2 = y->left;

  y->left  = x;
  x->right = T2;

  x->height = max(height(x->left), height(x->right)) + 1;
  y->height = max(height(y->left), height(y->right)) + 1;

  return y;
}

template < typename T > i64 getBalance(Table< T >* N) {
  if (N == 0)
    return 0;
  return height(N->left) - height(N->right);
}

template < typename T > Table< T >* insert(Table< T >* node, u64 key, u64 crc) {
  if (node == 0)
    return (create_id_to_crc64_map_node(key, crc));

  if (key < node->key) {
    node->left = insert(node->left, key, crc);
  } else if (key > node->key) {
    node->right = insert(node->right, key, crc);
  } else {
    return node;
  }
  node->height = 1 + max(height(node->left), height(node->right));

  i64 balance = getBalance(node);

  if (balance > 1 && key < node->left->key) {
    return right_rotate(node);
  }
  if (balance < -1 && key > node->right->key) {
    return left_rotate(node);
  }

  if (balance > 1 && key > node->left->key) {
    node->left = left_rotate(node->left);
    return right_rotate(node);
  }

  if (balance < -1 && key < node->right->key) {
    node->right = right_rotate(node->right);
    return left_rotate(node);
  }

  return node;
}

template < typename T > T* search(struct Table< T >* root, u64 key) {
  if (root == 0) {
    return 0;
  }
  if (root->key == key) {
    return &root->val;
  }

  if (root->key < key) {
    return search(root->right, key);
  }

  return search(root->left, key);
}

template < typename T > Table< T >* min_value_node(Table< T >* node) {
  Table< T >* current = node;

  while (current->left != 0)
    current = current->left;

  return current;
}

template < typename T > Table< T >* remove(Table< T >* root, i64 key) {

  if (root == 0) {
    return root;
  }

  if (key < root->key) {
    root->left = remove< T >(root->left, key);

  } else if (key > root->key) {
    root->right = remove< T >(root->right, key);
  } else {
    if ((root->left == 0) || (root->right == 0)) {
      Table< T >* temp = root->left ? root->left : root->right;

      if (temp == 0) {
        temp = root;
        root = 0;
      } else
        *root = *temp;

      free(temp);
    } else {
      Table< T >* temp = min_value_node< T >(root->right);

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

  return root;
}

} // namespace lib
