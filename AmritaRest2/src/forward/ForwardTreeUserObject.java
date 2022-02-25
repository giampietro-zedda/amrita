package forward;
import java.io.Serializable;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;

/**
 * copyright (c) 2009-2012 e-Amrita - Ing Giampietro Zedda   Turin (Italy)
 * 
 * <h1>
 * ForwardTreeUserObject
 * </h1>
 * <p>
 * It's a wrapper for a generic user object binded to a swing {@link JTree} object.<br>
 * It holds some informations in order to let, for example, a convenient way to get internationalization<br>
 * of node text but can manage any type of data.
 * <br>
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 09/dic/2011 
 * @see ForwardTreeModel
*/


public class ForwardTreeUserObject implements Serializable {
	
	public static final String SERVICE_NODE_TEXT = "**Service**";
	private static final long serialVersionUID = 1L;
 
	private DefaultMutableTreeNode nodeJTree = null;            // Oggetto nodo implementato da swing JTree
    private String nodeText = "";								// Testo nodo
    private String toolTipText = "";							// Testo toolTip
    private boolean isNodeWithAnyChild = false;					// True indica che il nodo deve essere creato come NON leaf con un figlio di servizio
    															// Alla prima richiesta di espansione verranno inseriti applicativamente i figli
                                                                // Il nodo figlio di servizio avrà object string "**Service Node**"
    private boolean isNodeService = false;						// True indica che questo è un nodo di servizio da rimpiazzare runtime con i child effettivi

     
    /**
     * Creates a standard user node data model, with the text specified.<br>
     * It rapresents user object data bound to tree node.<br>
     * Any more structured need of user data bound, must be realized <br>
     * by a user class that inherits from this.<br>
     * <p>
     * @param nodeText
     */
	public ForwardTreeUserObject(String nodeText) {
		this.nodeText = nodeText; 
	}


	
	
	/**
	 * Get the text displayed on the node<br>
	 * <p>
	 * @return the nodeText
	 */
	public String getNodeText() {
		return nodeText;
	}


	/**
	/**
	 * Set the text displayed on the node<br>
	 * <p>
	 * @param nodeText the nodeText to set
	 */
	public void setNodeText(String nodeText) {
		this.nodeText = nodeText;
	}


	/**
	 * Get the Swing JTree node as a {@link DefaultMutableTreeNode} object.<br>
	 * <p>
	 * @return the nodeJTree
	 */
	public DefaultMutableTreeNode getNodeJTree() {
		return nodeJTree;
	}




	/**
	 * Set the Swing JTree node as a {@link DefaultMutableTreeNode} object.<br> 
	 * <p>
	 * @param nodeJTree the nodeJTree to set
	 */
	public void setNodeJTree(DefaultMutableTreeNode nodeJTree) {
		this.nodeJTree = nodeJTree;
	}




	/**
	 * Get the  node tooltip text.<br>
	 * <p>
	 * @return the toolTipText
	 */
	public String getToolTipText() {
		return toolTipText;
	}


	/**
	 * Set the  node tooltip text.<br>
	 * <p>
	 * @param toolTipText the toolTipText to set
	 */
	public void setToolTipText(String toolTipText) {
		this.toolTipText = toolTipText;
	}

	/**
	 * Get if the node has any child to append.<br>
	 * <p>
	 * To prevent a potentially expensive initial tree loading,<br>
	 * tree children will be added to any parent just at the first node user expand request.<br>
	 * So, at initially declaration time, for the first level under the root node,<br>
	 * and then at any level at first node user expand request, all childreen will be phisically added.
	 * If the node has any child to append the right closed icon will be showed and a service, 
	 * hidden child node will be added.<br>
	 * Then, at the first node user expand request, the serice node will be removed and all user node<br>
	 * will be adde instead.<br>
	 * <p>
	 * @return the isNodeWithAnyChild
	 */
	public boolean isNodeWithAnyChild() {
		return isNodeWithAnyChild;
	}


	/**
	 * Set if the node has any child to append.<br>
	 * <p>
	 * To prevent a potentially expensive initial tree loading,<br>
	 * tree children will be added to any parent just at the first node user expand request.<br>
	 * So, at initially declaration time, for the first level under the root node,<br>
	 * and then at any level at first node user expand request, all childreen will be phisically added.
	 * If the node has any child to append the right closed icon will be showed and a service, 
	 * hidden child node will be added.<br>
	 * Then, at the first node user expand request, the serice node will be removed and all user node<br>
	 * will be adde instead.<br>
	 * <p>
	 * @param isNodeWithAnyChild the isNodeWithAnyChild to set
	 */
	public void setNodeWithAnyChild(boolean isNodeWithAnyChild) {
		this.isNodeWithAnyChild = isNodeWithAnyChild;
	}


	/**
	 * Get if the node is a service node, automatically inserted<br>
	 * the closed icon to be painted for<br>
	 * 
	 * @return the isNodeService
	 */
	public boolean isNodeService() {
		return isNodeService;
	}




	/**
	 * Set if the node is a service node, automatically inserted<br>
	 * the closed icon to be painted for<br>
	 * 
	 * @param isNodeService the isNodeService to set
	 */
	public void setNodeService(boolean isNodeService) {
		this.isNodeService = isNodeService;
	}




	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return nodeText;
	}
	
	
 }
