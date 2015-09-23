package bst;
import java.awt.Color;
import drawing.*;

public class BSTVisualizer {
	private DrawingArea canvas;

	// diameter of nodes;
	private final static int DIAMETER = 15;
	// horizontal distance  between nodes on same level
	private final static int HORIZONTAL_DIST = 1;
	// vertical distance between levels
	private final static int VERTICAL_DIST = 10;
	// distance between node (circle) center and content string 
	private final static int OFFSET = -10;

	/**
	 * Creates a canvas with a certain title, width, height.
	 */
	public BSTVisualizer(String title, int width, int height) {
		canvas = new DrawingArea(title, width, height, Color.white);
	}

	/**
	 * Draws a binary search tree on the canvas.
	 */
	 public void drawTree(BinarySearchTree<?> bst) {
		 if (bst.root != null) {
			 canvas.erase();
			 int h = bst.height();
			 int rootNbr = (int) Math.pow(2,h);
			 drawTree(bst.root, 1, h, rootNbr);
			 canvas.paint();
		 }
	 }

	/* ------   Private auxiliary methods --------------*/

	private void drawTree(BinarySearchTree.BinaryNode<?> n, int level, int h, int rootNbr) {
		int xPos = computeXpos(rootNbr);
		int yPos = computeYpos(level);
		int childYpos = computeYpos(level+1);

		if (n.left != null) {
			int leftChildRootNbr = rootNbr - (int) Math.pow(2,h-1);
			drawTree(n.left, level+1,h-1, leftChildRootNbr);
			int leftChildXpos = computeXpos(leftChildRootNbr);
			canvas.drawLine(Color.BLACK, xPos,yPos,leftChildXpos,childYpos);
		}
		canvas.fillCircle(Color.BLUE,xPos, yPos, DIAMETER);
		canvas.drawString(Color.BLACK, n.element.toString(), xPos+OFFSET, yPos+OFFSET);	
		if (n.right != null) {
			int rightChildRootNbr = rootNbr + (int) Math.pow(2,h-1);
			drawTree(n.right, level+1,h-1, rightChildRootNbr);
			int rightChildXpos = computeXpos(rightChildRootNbr);
			canvas.drawLine(Color.BLACK, xPos,yPos,rightChildXpos,childYpos);
		}
	}

	/* Compute y-position of a node from its level. */
	private int computeYpos(int level) {
		return level*(DIAMETER+VERTICAL_DIST);
	}

	/* Compute x-position of a node from its inordernumber. */
	private int computeXpos(int actNodeNbr) {
		return actNodeNbr*(DIAMETER + HORIZONTAL_DIST);
	}
    
}
