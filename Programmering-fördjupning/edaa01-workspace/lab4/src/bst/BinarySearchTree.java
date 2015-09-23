package bst;



public class BinarySearchTree<E extends Comparable<? super E>> {
	public static void main (String [] args){
		BinarySearchTree <Integer> tree = new BinarySearchTree <Integer> ();		
		
		
		
		tree.add(1);
		tree.add(2);
		tree.add(3);
		tree.add(6);
		tree.add(7);
		tree.add(9);
		tree.add(10);
		tree.add(11);
		tree.add(99);
		tree.add(4);
		
			
		BSTVisualizer BST = new BSTVisualizer ("test", 1200, 600);
		tree.rebuild();
		BST.drawTree(tree);
		
		
		System.out.println(tree.size());
		
		
		System.out.println(tree.height());
		
		

		
		
		
	}
	
	BinaryNode<E> root;
    int size;
    
    
	/**
	 * Constructs an empty binary searchtree.
	 */
	public BinarySearchTree() {
		root=null;
		
	}

	/**
	 * Inserts the specified element in the tree if no duplicate exists.
	 * @param x element to be inserted
	 * @return true if the the element was inserted
	 */
	public boolean add(E x) {
		
		if (root == null) {
			root= new BinaryNode<E> (x);
			size++;
			return true;
			
		}
		
		return add(x, root);
		
		
		
		
		
	}
	
	private boolean add(E x, BinaryNode<E> n) {
		
	
		int compare = n.element.compareTo(x);
		
		if(compare == 0) {
			return false;
		}
		
		else if(compare < 0) {
			
			
			if(n.right == null) {
				
				n.right = new BinaryNode <E> (x);
				size++;
				return true;
			}
			
						
			else {
			
				add(x, n.right);
			}
		}
		
			else if (compare > 0){
			
			if(n.left == null) {
				
				n.left = new BinaryNode <E> (x);
				size++;
				return true;
			}
			

			else {
				add(x, n.left);
			}
		}
		return false;
		
		
		
		
	}
	
	/**
	 * Computes the height of tree.
	 * @return the height of the tree
	 */
	public int height() {
		
		return height(root);
		

		
		
		
	}
		
	private int height(BinaryNode<E> node){
		if (node == null){
			return 0;
		}
		else{
			int heightL = 1+ height(node.left);
			int heightR = 1+ height(node.right);
			if(heightL > heightR){
				return heightL;
				
			}
			else{
				return heightR;
		}
		
		}
		  
	}
	

	/**
	 * Returns the number of elements in this tree.
	 * @return the number of elements in this tree
	 */
	public int size() {
		return size;
	}
	
	/**
	 * Print tree contents in inorder.
	 */
	public void printTree() {
		
		if(root == null) {
			System.out.println("The tree is empty!");
		}
		
		print(root);
		

	}
	
	private void  print (BinaryNode<E> n) {
		
		if(n!=null) {
			
			print(n.left);
			
			System.out.println(n.element.toString());
			
			print(n.right);
			
			
				
		}
				
	}

	/** 
	 * Builds a complete tree from the elements in the tree.
	 */
	public void rebuild() {
		if(root==null) {
			System.out.println("The tree is empty!");
				
		}
		
		E[] a = (E[]) new Comparable[size];
		int index = toArray(root, a, 0) -1;
		root = buildTree(a, 0, index);

	}
	
			
		
	
	
	/*
	 * Adds all elements from the tree rooted at n in inorder to the array a
	 * starting at a[index].
	 * Returns the index of the last inserted element + 1 (the first empty
	 * position in a).
	 */
	private int toArray(BinaryNode<E> n, E[] a, int index) {
	
		
		if(n!=null) {
			
			index = toArray(n.left,a,index);
			
			
			a[index] = n.element;
			index++;
			index = toArray(n.right,a,index);
				
		}
		return index;
	}
	
	/*
	 * Builds a complete tree from the elements a[first]..a[last].
	 * Elements in the array a are assumed to be in ascending order.
	 * Returns the root of tree.
	 */
	private BinaryNode<E> buildTree(E[] a, int first, int last) {
		if(first == last){
			return new BinaryNode<E> (a[last]);
		}
		else if(first <last){
			int middle = (last+first)/2;
			BinaryNode<E> node = new BinaryNode<E>(a[middle]);
			node.left = buildTree(a,first, middle-1);
			node.right = buildTree(a,middle+1, last);
			return node;
		}
		return null;
	}
	


	static class BinaryNode<E> {
		E element;
		BinaryNode<E> left;
		BinaryNode<E> right;

		private BinaryNode(E element) {
			this.element = element;
		}	
	}
	
}
