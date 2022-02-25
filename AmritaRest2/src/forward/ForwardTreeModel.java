package forward;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;


import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

/**
 * copyright (c) 2009-2012 e-Amrita - Ing Giampietro Zedda   Turin (Italy)
 * 
 * <h1>
 * ForwardTreeModel
 * </h1>
 * <p>
 * Keep all necessary model data for a tre.<br>
 * <p>
 * Manages nodes get, insert and remove and other convenient methods.<br>. 
 * This class extends the standard swing model class DefaultTreeModel, but all available methods are referred
 * directly to node user objects, instead of {@link MutableTreeNode} objects of standard model.<br>
 * This approach let not to have swing JTree obbject coupled with the forward implementation, desktop or web.<br>
 * Application can refer so always to application objects nodes.<br>
 * So the major part of methods available at {@link DefaultTreeModel} level and at {@link DefaultMutableTreeNode}, have been mapped<br>
 * in this class and, not to make confusion, are starting with underscore ("_")<br>
 * Anyway, if it needs, its possible to access to original methods but it's deprecated.<br>
 * With java method <code>getChild()</code> you get the {@link DefaultMutableTreeNode}<br>
 * and on this object its possible to call any standard java method.<br>
 * Forward monitor makes available to application code, when tree events occurred,  {@link DefaultMutableTreeNode} objects involved<br>
 * to be used as input to several methods.<br>
 * <p>
 * This class holds additional data for a {@link JTree} swing control useful for applications and completely user dependent.<br>
 * For example, to avoid the complete tree population at inizialization time, here are managed nodes informations for the first<br>
 * tree level, just under the root node.<br>
 * For each first level node, it will be stored the information if the node will be leaf or not.<br>
 * For nodes declared not to be leaf, will be inserted a service child, just to make visible the folder icon.<br>
 * At first node expand request the service node will be removed and all correct nodes will be added as child.<br>
 * <br> 
 * Because this class inherits from {@link DefaulTreeModel}, all specific forward methods are starting with _ (underscore).<br>
 * <p>
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 09/dic/2011 
 *
*/

public class ForwardTreeModel extends DefaultTreeModel implements Serializable {
	/**
	  The tree will be initially with no node expanded
	 */
	public static int INITIAL_EXPAND_NONE = 0;
	/**
	  The tree will be initially with the only root node expanded, just one level
	 */
	public static int INITIAL_EXPAND_ROOT = 0;
	/**
	  The tree will be initially with all nodes expanded, at any level
	 */
	public static int INITIAL_EXPAND_ALL = 0;
	
	
	
	private static final long serialVersionUID = 1L;
 
	// Oggetto Swing
	private JTree jtree = null;														// Reference a oggetto
	
	// Initial expand mode
	private int initialExpandMode = 0;                                              // INITIAL_EXPAND_NONE | INITIAL_EXPAND_ROOT | INITIAL_EXPAND_ALL
	
	// Root
	private ForwardTreeUserObject rootNode = null;									// Nodo root, descrizione e altre informazioni nell'oggetto
    
    // Map key id not, oggetto applicativo nodo, valorizzata da DATA_BOUND()
    private Map<String, ForwardTreeUserObject> hm_node = null;                      // Key=id nodo, Data=user object applicativo
                                                                                    //   id nodo = level + "-" + position, per root = "0", poi "0-0", "0-1", "0-1-0" etc.
    // Valori iniziali tree valorizzati da DATA_VALUES() in function declarating
    private Object ar_initialDescriptor[] = null;									// Struttura iniziale dichiarata con DATA_VALUES()
    
    // Primo livello, descritto separatamente
    private ArrayList<ForwardTreeUserObject> al_nodeFirstLevel = null;				// Nodi di primo livello sotto la root
    private ArrayList<ForwardTreeUserObject> al_nodeFirstLevelObject = null;		// Oggetti utente binded ai nodi di primo livello
    private ArrayList<Boolean> al_nodeFirstLevelLeaf = null;						// Se elementi false si crea un figlio di servizio
    
    // Icone, assegnate come Icon dal monitor solo runtime in fase di composizione GUI
    private String iconPathLeaf = "";												// Path icona su nodo foglia
    private String iconPathOpen = "";												// Path icona su nodo aperto				
    private String iconPathClosed = "";												// Path icona su nodo chiuso
    
    // Info varie
    private int selectionMode = 0;                                                  // TreeSelectionModel.SINGLE_TREE_SELECTION | CONTIGUOUS_TREE_SELECTION | DISCONTIGUOUS_TREE_SELECTION 
    private boolean isEditable = false;                                             // True indica testo nodi editabile
    private boolean showsRootHandles = false;                                       // True show la root completa
    
    
  	/**
     * Creates a tree model with the specified root node user object.<br>
     * <p>
     * A {@link DefaultMutableTreeNode} will be generated for the root node to initialize the tree model.<br>
     * <p>
 	 * @param rootNodeUserObject as a user node object that inherits from {@link ForwardTreeUserObject}
 	 */
	public ForwardTreeModel(JTree jtree, ForwardTreeUserObject rootNodeObject) {
		super(rootNodeObject.getNodeJTree());
		this.jtree = jtree; 
		this.rootNode = rootNodeObject;
		this.hm_node = new HashMap<String, ForwardTreeUserObject>();
	}
 

	/**
	 * Get the swing JTree object owner.<br>
	 * <p>
	 * @return the jtree
	 */
	public JTree _getJtree() {
		return jtree;
	}


	/**
	 * Get the swing JTree object owner of this model.<br>
	 * <p>
	 * @param jtree the jtree to set
	 */
	public void _setJtree(JTree jtree) {
		this.jtree = jtree;
	}


	/**
	 * Get the root node as a {@link ForwardTreeUserObject} object.<br>
	 * <p>
	 * @return the rootNode
	 */
	public ForwardTreeUserObject _getRootNode() {
		return rootNode;
	}


	/**
	 * Set the root node as a {@link ForwardTreeUserObject} object.<br>
	 * <p>
	 * @param rootNode the rootNode to set
	 */
	public void _setRootNode(ForwardTreeUserObject rootNode) {
		this.rootNode = rootNode;
	}


	/**
	 * Get the initial expansion mode as a {@link ForwardTreeModel} constants.<br>
	 * <p>
	 * Good values are:<pre>
		INITIAL_EXPAND_NONE
		INITIAL_EXPAND_ROOT
		INITIAL_EXPAND_ALL<pre>
	 * @return the initialExpandMode
	 */
	public int _getInitialExpandMode() {
		return initialExpandMode;
	}


	/**
	 * Set the initial expansion mode as a {@link ForwardTreeModel} constants.<br>
	 * <p>
	 * Good values are:<pre>
		INITIAL_EXPAND_NONE
		INITIAL_EXPAND_ROOT
		INITIAL_EXPAND_ALL<pre>
	 * @param initialExpandMode the initialExpandMode to set
	 */
	public void _setInitialExpandMode(int initialExpandMode) {
		this.initialExpandMode = initialExpandMode;
	}


	/**
	 * Get the map of nodes in the tree.<br>
	 * <p>
	 * The map is automatically defined for the tree model.<br>
	 * Using the key id identifier as key, it's possible to get<br>
	 * the {@link ForwardTreeUserObject} bound to the node.<br>
	 * The key id is always as <code>posInlevelo"-"posInlevel1"-"....posInLeveln<code><br>
	 * Any "-" leads to a deeper tree level.<br>
	 * Here is an example;<pre>
	 * "1-4-2-5"
	 * 
	 * to address the child 0-based at position 5 four levels under the root.
	 * 
	 * The nodes hierarchy is:
	 *    Node1 at position 1 0-based of root children
	 *    Node2 at position 4 0-based of node1 children
	 *    Node3 at position 2 0-based of node2 children
	 *    Node4 at position 5 0-based of node3 children as the desired node
	 * </pre>
	 * The root key id is always the "0" string.<br>
	 * <p>
	 * If no key id has been declared, a null value will be returned.<br>
	 * <p>
	 * @return the {@link ForwardTreeUserObject} node object
	 */
	public Map<String, ForwardTreeUserObject> _getMapNodes() {
		return hm_node;
	}


	/**
	 * Get the initial hierarchy tree descriptor as declarede by DATA_VALUES() section at function declaration time.<br>
	 * <br>
	 * The descriptor takes the form of an objects array arranged recursively to get the complete tree hierarchy descriptor.<br>
	 * The best way to understand how the descriptor works, is to take a look to the example below:<pre>
	 * new Object[]{new Object[]{"Node1", "0-0"}                       // Node1 leaf 
	 *             ,new Object[]{"Node2", "0-1",                       // Node2 folder 
	 *                                    new Object[]{"Node21", "0-2-0"		// .. 
	 *                                                ,"Node22", "0-2-1"		// ..
	 *                                                ,"Node23", "0-2-2"		// ..
	 *                                                ,"Node24", "0-2-3"		//	Nodes child of Node2  
	 *                                                ,"Node25", "0-2-4"		// ..
	 *                                                ,"Node26", "0-2-5"		// ..
	 *                                                ,"Node27", "0-2-6"		// ..
	 *                                                }
	 *              									  
	 *                          }
	 *             ,new Object[]{"Node3", "0-2", true}                 // Node3 folder with first service child automatically inserted
	 *             ,new Object[]{"Node4", "0-2",                       // Node4 folder  
	 *                                    new Object[]{"Node41", "0-4-0"	   		// ..
	 *                                                ,"Node42", "0-4-1"	   		// ..
	 *                                                ,"Node43", "0-4-2"	   		// Nodes child of Node4 
	 *                                                ,"Node44", "0-4-3"	   		// .
	 *                                                ,"Node45", "0-4-4"	   		// ..
	 *                                                }
	 *                          }
	 *             }
	 * </pre>
	 * Notice that strings like "0-1", "0-4-1" and so on, are the key id to identify the node.<br>
	 * <p>
	 * @return the ar_initialDescriptor
	 */
	public Object[] _getInitialDescriptor() {
		return ar_initialDescriptor;
	}


	/**
	 * @param ar_initialDescriptor the ar_initialDescriptor to set
	 */
	public void setInitialDescriptor(Object[] ar_initialDescriptor) {
		this.ar_initialDescriptor = ar_initialDescriptor;
	}


	/**
	 * Get first level nodes, immediately under the root node.<br>
	 * <p>
	 * @return the al_nodeFirstLevel
	 */
	public ArrayList<ForwardTreeUserObject> _getNodesFirstLevel() {
		return al_nodeFirstLevel;
	}


	/**
	 * Set first level nodes, immediately under the root node.<br>
	 * <p>
	 * @param al_nodeFirstLevel the al_nodeFirstLevel to set
	 */
	public void _setNodesFirstLevel(ArrayList<ForwardTreeUserObject> al_nodeFirstLevel) {
		this.al_nodeFirstLevel = al_nodeFirstLevel;
	}


	/**
	 * Get first level user objects nodes, immediatly under the root node.<br>
	 * <p>
	 * @return the al_nodeFirstLevelObject
	 */
	public ArrayList<ForwardTreeUserObject> _getNodesFirstLevelObject() {
		return al_nodeFirstLevelObject;
	}


	/**
	 * Set first level user objects nodes, immediatly under the root node.<br>
	 * <p>
	 * @param al_nodeFirstLevelObject the al_nodeFirstLevelObject to set
	 */
	public void _setNodesFirstLevelObject(ArrayList<ForwardTreeUserObject> al_nodeFirstLevelObject) {
		this.al_nodeFirstLevelObject = al_nodeFirstLevelObject;
	}


	/**
	 * Get if any node at the first level is a leaf or will have children.<b>
	 * For optimization al parent child nodes will be added on demand of node expand.<br>
	 * So if the node will have children will be added a service node, just to paint<br>
	 * the Closed icon.<br>
	 * <p>
	 * @return the al_nodeFirstLevelLeaf
	 */
	public ArrayList<Boolean> _getNodesFirstLevelLeaf() {
		return al_nodeFirstLevelLeaf;
	}


	/**
	 * Set if any node at the first level is a leaf or will have children.<b>
	 * For optimization al parent child nodes will be added on demand of node expand.<br>
	 * So if the node will have children will be added a service node, just to paint<br>
	 * the Closed icon.<br>
	 * <p>
	 * @param al_nodeFirstLevelLeaf the al_nodeFirstLevelLeaf to set
	 */
	public void _setNodesFirstLevelLeaf(ArrayList<Boolean> al_nodeFirstLevelLeaf) {
		this.al_nodeFirstLevelLeaf = al_nodeFirstLevelLeaf;
	}


	/**
	 * Get the leaf path icon that will be active at UI inizialization.<br>
	 * <p>
	 * A null value or an empty string causes the default icon to be used.<br>
	 * <p>
	 * @return the iconPathLeaf
	 */
	public String _getIconPathLeaf() {
		return iconPathLeaf;
	}


	/**
	 * Set the leaf path icon that will be active at UI inizialization.<br>
	 * <p>
	 * A null value or an empty string causes the default icon to be used.<br>
	 * <p>
	 * @param iconPathLeaf the iconPathLeaf to set
	 */
	public void _setIconPathLeaf(String iconPathLeaf) {
		this.iconPathLeaf = iconPathLeaf;
	}


	/**
	 * Get the open path icon that will be active at UI inizialization.<br>
	 * <p>
	 * A null value or an empty string causes the default icon to be used.<br>
	 * <p>
	 * @return the iconPathOpen
	 */
	public String _getIconPathOpen() {
		return iconPathOpen;
	}


	/**
	 * Set the open path icon that will be active at UI inizialization.<br>
	 * <p>
	 * A null value or an empty string causes the default icon to be used.<br>
	 * <p>
	 * @param iconPathOpen the iconPathOpen to set
	 */
	public void _setIconPathOpen(String iconPathOpen) {
		this.iconPathOpen = iconPathOpen;
	}


	/**
	 * Get the Closed path icon that will be active at UI inizialization.<br>
	 * <p>
	 * A null value or an empty string causes the default icon to be used.<br>
	 * <p>
	 * @return the iconPathClosed
	 */
	public String _getIconPathClosed() {
		return iconPathClosed;
	}


	/**
	 * Set the Closed path icon that will be active at UI inizialization.<br>
	 * <p>
	 * A null value or an empty string causes the default icon to be used.<br>
	 * <p>
	 * @param iconPathClosed the iconPathClosed to set
	 */
	public void _setIconPathClosed(String iconPathClosed) {
		this.iconPathClosed = iconPathClosed;
	}


	/**
	 * Get the selection mode of tree nodes<br>
	 * <p>
	 * 
	 * @return the selectionMode as a {@link TreeSelectionModel} constant like<pre>
	 * SINGLE_TREE_SELECTION 
	 * CONTIGUOUS_TREE_SELECTION 
	 * DISCONTIGUOUS_TREE_SELECTION <pre>
	 * <p>
	 */
	public int _getSelectionMode() {
		return selectionMode;
	}


	/**
	 * Get the selection mode of tree nodes<br>
	 * <p>
	 * @param selectionMode the selectionMode to set as a {@link TreeSelectionModel} constant like<pre>
	 * SINGLE_TREE_SELECTION 
	 * CONTIGUOUS_TREE_SELECTION 
	 * DISCONTIGUOUS_TREE_SELECTION <pre>
	 */
	public void _setSelectionMode(int selectionMode) {
		this.selectionMode = selectionMode;
	}


	/**
	 * Get if node text is editable.<br>
	 * <p>
	 * @return the isEditable
	 */
	public boolean _isEditable() {
		return isEditable;
	}


	/**
	 * Set if node text is editable.<br>
	 * <p>
	 * @param isEditable the isEditable to set
	 */
	public void _setEditable(boolean isEditable) {
		this.isEditable = isEditable;
	}


	/**
	 * Get if the root node is painted in extended way or without left margin.<br>
	 * <p>
	 * @return the showsRootHandles
	 */
	public boolean _isShowsRootHandles() {
		return showsRootHandles;
	}


	/**
	 * Set if the root node is painted in extended way or without left margin.<br>
	 * <p>
	 * @param showsRootHandles the showsRootHandles to set
	 */
	public void _setShowsRootHandles(boolean showsRootHandles) {
		this.showsRootHandles = showsRootHandles;
	}
	
	/**
	 * Insert a child node at the specific position, 0-based.<br>
	 * <p>
	 * If the input parent node is null no action will be taken.<br>
	 * <p>
	 * @param nodeParent as a user object that inherits from {@link ForwardTreeUserObject} 
	 * @param nodeChild as a user object that inherits from {@link ForwardTreeUserObject} 
	 * @param indexAt the 0-based index where to insert the child node
	 * @param isNodeWithAnyChild true causes a servize node to be added to nodeChild to let closed icon to be painted 
	 * @param immediatlyVisible true makes sure the user can see the new node.
	 */
	public void _insertNodeChildAt(Object nodeParent, Object nodeChild, int indexAt, boolean isNodeWithAnyChild, boolean immediatelyVisible) {
		if (nodeParent == null) { return;}
		insertNodeCommon(nodeParent, nodeChild, indexAt, isNodeWithAnyChild, immediatelyVisible); 		// Inserimento comune nodo
		return;
	}


	/**
	 * Add a child node as the last child.<br>
	 * <p>
	 * @param nodeParent as a user object that inherits from {@link ForwardTreeUserObject}
	 * @param nodeChild as a user object that inherits from {@link ForwardTreeUserObject}
	 * @param isNodeWithAnyChild true causes a service node to be added to nodeChild to let closed icon to be painted 
	 * @param immediatelyVisible true makes sure the user can see the new node.
	 */
	public void _addNodeChild(Object nodeParent, Object nodeChild, boolean isNodeWithAnyChild, boolean immediatelyVisible) {
		
		ForwardTreeUserObject nodeParentUserObject = null;
		DefaultMutableTreeNode nodeParentTree = null;
		int index = 0;
		
		// Recupero numero child DefaultMutableTreeNode parent
		nodeParentUserObject = (ForwardTreeUserObject) nodeParent;
		nodeParentTree = nodeParentUserObject.getNodeJTree();
		index = getChildCount(nodeParentTree);
		
		// Inserimento comune nodo
		insertNodeCommon(nodeParent, nodeChild, index, isNodeWithAnyChild, immediatelyVisible);
		return;
		
	}

	/*
	 * Codice comune di inserimento nodo in posizione specifica
	 */
	private void insertNodeCommon(Object nodeParent, Object nodeChild, int indexAt, boolean isNodeWithAnyChild, boolean immediatelyVisible) {
		
		ForwardTreeUserObject nodeParentUserObject = null;
		ForwardTreeUserObject nodeChildUserObject = null;
		ForwardTreeUserObject nodeChildServiceObject = null;
		ForwardTreeUserObject nodeChildWorkObject = null;
		DefaultMutableTreeNode nodeParentTree = null;
		DefaultMutableTreeNode nodeChildTree = null;
		DefaultMutableTreeNode nodeChildServiceTree = null;
		DefaultMutableTreeNode nodeChildWorkTree = null;
		int index = 0;
		
		// Controllo di sicurezza 
		if (!(nodeParent instanceof ForwardTreeUserObject) 
		||  !(nodeChild  instanceof ForwardTreeUserObject)) {
			return;
		}
		
		// Recupero oggetto user nodo parent e child
		nodeParentUserObject = (ForwardTreeUserObject) nodeParent;
		nodeChildUserObject = (ForwardTreeUserObject) nodeChild;
		nodeParentTree = nodeParentUserObject.getNodeJTree();
		
		// Se primo nodo child di servizio viene rimosso prima di ogni altra operazione
		index = getChildCount(nodeParentTree);
		if (index > 0) {
			nodeChildWorkTree = (DefaultMutableTreeNode) getChild(nodeParentTree, 0);
			nodeChildWorkObject = (ForwardTreeUserObject) nodeChildWorkTree.getUserObject();
			if (nodeChildWorkObject.isNodeService()) {
				this.removeNodeFromParent(nodeChildWorkTree);
				index = getChildCount(nodeParentTree);
			}
		}
		
		// Se indexAt > numero children forzo a numero children
		if (indexAt < index) {
			index = indexAt;				// Si garantisce di accodare il child
		}
		
		// Creazione e inserimento nodo swing child  con possibilità che abbia figli 
		nodeChildTree = new DefaultMutableTreeNode(nodeChildUserObject, true);
		nodeChildUserObject.setNodeJTree(nodeChildTree);
		insertNodeInto(nodeChildTree, nodeParentTree, index);
        if (immediatelyVisible) {
            this.jtree.scrollPathToVisible(new TreePath(nodeChildTree.getPath()));
        }
		
		// Nodo sicuramente senza figli: nessuna operazione
		if (!isNodeWithAnyChild) {return;}
		
		// Inserimento nodo di servizio figlio di nodo child
		nodeChildServiceObject = new ForwardTreeUserObject(ForwardTreeUserObject.SERVICE_NODE_TEXT);
		nodeChildServiceTree = new DefaultMutableTreeNode(nodeChildServiceObject, false);
		nodeChildServiceObject.setNodeJTree(nodeChildServiceTree);
		nodeChildServiceObject.setNodeService(true);
		insertNodeInto(nodeChildServiceTree, nodeChildTree, 0);
	}


	/**
	 * Add a service child node to let the closed icon to be painted.<br>
	 * <p>
	 * Parent children will be added by application at the first expand event.<br>
	 * <p>
	 * This method is intended for forward internal use only.<br>
	 * <p>
	 * @param nodeParent as a user object that inherits from {@link ForwardTreeUserObject} 
	 */
	public void _addNodeChildService(Object nodeParent) {
		ForwardTreeUserObject nodeParentUserObject = null;
		ForwardTreeUserObject nodeChildServiceObject = null;
		DefaultMutableTreeNode nodeParentTree = null;
		DefaultMutableTreeNode nodeChildServiceTree = null;
		
		// Controllo di sicurezza 
		if (!(nodeParent instanceof ForwardTreeUserObject)) {
			return;
		}
		
		// Recupero nodo DefaultMutableTreeNode parent
		nodeParentUserObject = (ForwardTreeUserObject) nodeParent;
		nodeParentTree = nodeParentUserObject.getNodeJTree();

		// Inserimento nodo di servizio figlio di parent
		nodeChildServiceObject = new ForwardTreeUserObject(ForwardTreeUserObject.SERVICE_NODE_TEXT);
		nodeChildServiceTree = new DefaultMutableTreeNode(nodeChildServiceObject, false);
		nodeChildServiceObject.setNodeJTree(nodeChildServiceTree);
		insertNodeInto(nodeChildServiceTree, nodeParentTree, 0);
		return;
	}
	

	/**
	 * Remove the first parent child.<br>
	 * <p>
	 * If nodeParent is null or if there is no child, no action will be taken.<br>
	 * <p>
	 * @param nodeParent as a user object that inherits from {@link ForwardTreeUserObject}
	 */
	public void _removeNodeChildFirst(Object nodeParent) {

		ForwardTreeUserObject nodeParentUserObject = null;
		DefaultMutableTreeNode nodeParentTree = null;
		DefaultMutableTreeNode nodeChildFirstTree = null;
		
		// Controlli di sicurezza 
		if (nodeParent == null) {return;}
		if (!(nodeParent instanceof ForwardTreeUserObject)) {return;}
		
		// Recupero oggetto nodo parent  
		nodeParentUserObject = (ForwardTreeUserObject) nodeParent;
		nodeParentTree = nodeParentUserObject.getNodeJTree();
		
		// Parent senza child: nessuna operazione
		if (getChildCount(nodeParentTree) == 0) {
			return;
		}
		
		// Remove first child
		nodeChildFirstTree = (DefaultMutableTreeNode) getChild(nodeParentTree, 0);
		this.removeNodeFromParent(nodeChildFirstTree);
		
		return;
	}


	/**
	 * Remove the last parent child.<br>
	 * <p>
	 * If nodeParent is null or if there is no child, no action will be taken.<br>
	 * <p>
	 * @param nodeParent as a user object that inherits from {@link ForwardTreeUserObject} 
	 */
	public void _removeNodeChildLast(Object nodeParent) {

		ForwardTreeUserObject nodeParentUserObject = null;
		DefaultMutableTreeNode nodeParentTree = null;
		DefaultMutableTreeNode nodeChildFirstTree = null;
		
		// Controlli di sicurezza 
		if (nodeParent == null) {return;}
		if (!(nodeParent instanceof ForwardTreeUserObject)) {return;}
		
		// Recupero oggetto nodo parent  
		nodeParentUserObject = (ForwardTreeUserObject) nodeParent;
		nodeParentTree = nodeParentUserObject.getNodeJTree();
		
		// Parent senza child: nessuna operazione
		if (getChildCount(nodeParentTree) == 0) {
			return;
		}
		
		// Remove last child
		nodeChildFirstTree = (DefaultMutableTreeNode) getChild(nodeParentTree, getChildCount(nodeParentTree) - 1);
		this.removeNodeFromParent(nodeChildFirstTree);
		
		return;
	}

	/**
	 * Remove all parent children.<br>
	 * <p>
	 * If nodeParent is null  no action will be taken.<br>
	 * <p>
	 * @param nodeParent as a user object that inherits from {@link ForwardTreeUserObject}
	 */
	public void _removeNodeChildAll(Object nodeParent) {
		
		ForwardTreeUserObject nodeParentUserObject = null;
		DefaultMutableTreeNode nodeParentTree = null;
		
		// Controlli di sicurezza 
		if (nodeParent == null) {return;}
		if (!(nodeParent instanceof ForwardTreeUserObject)) {return;}
		
		// Recupero oggetto nodo parent  
		nodeParentUserObject = (ForwardTreeUserObject) nodeParent;
		nodeParentTree = nodeParentUserObject.getNodeJTree();

		// Remove last child
		nodeParentTree.removeAllChildren();
		
		return;
	}

	/**
	 * Remove the node from his parent.<br>
	 * <p>
	 * @param node as a user object that inherits from {@link ForwardTreeUserObject}
	 */
	public void _removeNode(Object node) {

		ForwardTreeUserObject nodeObject = null;
		DefaultMutableTreeNode nodeTree = null;

		// Controlli di sicurezza, di esistenza nodo e di aggancio a un parent 
		if (node == null) {return;}
		if (!(node instanceof ForwardTreeUserObject)) {return;}
		nodeObject = (ForwardTreeUserObject) node;
		nodeTree = nodeObject.getNodeJTree();
		if (nodeTree.getParent() == null) {return;}

		// Nodo esistente, della classe corretta e agganciato a un parent
        this.removeNodeFromParent(nodeTree);
		return;
	}


	/**
	 * Remove the parent child at the specific position, 0-based.<br>
	 * <p>
	 * If the index of child to delete is out of range, no action will be taken.<br>
	 * <p>
	 * @param nodeParent as a user object that inherits from {@link ForwardTreeUserObject}
	 * @param index as a 0-based index of the parent child node to remove
	 */
	public void _removeNodeChildAt(Object nodeParent, int index) {

		ForwardTreeUserObject nodeParentUserObject = null;
		DefaultMutableTreeNode nodeParentTree = null;
		DefaultMutableTreeNode nodeChildTree = null;
		int countChild = 0;
		
		// Controllo di sicurezza 
		if (!(nodeParent instanceof ForwardTreeUserObject)) {return;}
		
		// Recupero oggetto user e tree  nodo parent 
		nodeParentUserObject = (ForwardTreeUserObject) nodeParent;
		nodeParentTree = nodeParentUserObject.getNodeJTree();
		
		// Index fornito out of range: nessuna operazione
		countChild = getChildCount(nodeParentTree);
		if (index + 1> countChild) {
			return;
		}
		
		// remove con metodo java standard
		nodeChildTree = (DefaultMutableTreeNode) getChild(nodeParentTree, index);
		removeNodeFromParent(nodeChildTree);										
		return;
	}


	/**
	 * Remove all tree nodes except the root node.<br>
	 * <p>
	 */
	public void _clear() {
		DefaultMutableTreeNode nodeRootTree = null;

		nodeRootTree = this.rootNode.getNodeJTree();
		nodeRootTree.removeAllChildren();
        this.reload();
		return;
	}

	/**
	 * Get the first child of the specified node.<br>
	 * <p>
	 * If the user object of the first child is not instance of {@link ForwardTreeUserObject} a null value will be returned.<br>
	 * <p>
	 * @param nodeParent as a user object that inherits from {@link ForwardTreeUserObject}
	 * @return the first child as a user object that inherits from {@link ForwardTreeUserObject}
	 */
	public ForwardTreeUserObject _getNodeChildFirst(ForwardTreeUserObject nodeParent) {
		
		ForwardTreeUserObject nodeParentUserObject = null;
		DefaultMutableTreeNode nodeParentTree = null;
		DefaultMutableTreeNode nodeChildTree = null;
		
		// Controllo di sicurezza 
		if (!(nodeParent instanceof ForwardTreeUserObject)) {return null;}
		
		// Recupero oggetto user e tree  nodo parent 
		nodeParentUserObject = (ForwardTreeUserObject) nodeParent;
		nodeParentTree = nodeParentUserObject.getNodeJTree();
		
		// No child: nessuna operazione
		if (getChildCount(nodeParentTree) == 0) {return null;}
		
		// Oggetto corretto
		nodeChildTree = (DefaultMutableTreeNode) nodeParentTree.getChildAt(0);
		if (nodeChildTree.getUserObject() instanceof ForwardTreeUserObject) {
			return (ForwardTreeUserObject) nodeChildTree.getUserObject();
		}
		
		// Nodo con oggetto utente non conforme
		return null;
	}

	/**
	 * Get the last child of the specified node.<br>
	 * <p>
	 * @param nodeParent as a user object that inherits from {@link ForwardTreeUserObject}
	 * @return the last child as a user object that inherits from {@link ForwardTreeUserObject}
	 */
	public ForwardTreeUserObject _getNodeChildLast(ForwardTreeUserObject nodeParent) {
		
		ForwardTreeUserObject nodeParentUserObject = null;
		DefaultMutableTreeNode nodeParentTree = null;
		DefaultMutableTreeNode nodeChildTree = null;
		int countChild = 0;
		
		// Controllo di sicurezza 
		if (!(nodeParent instanceof ForwardTreeUserObject)) {return null;}
		
		// Recupero oggetto user e tree  nodo parent 
		nodeParentUserObject = (ForwardTreeUserObject) nodeParent;
		nodeParentTree = nodeParentUserObject.getNodeJTree();
		
		// No child: nessuna operazione
		countChild = getChildCount(nodeParentTree);
		if (countChild == 0) {return null;}
		
		// Oggetto corretto
		nodeChildTree = (DefaultMutableTreeNode) nodeParentTree.getChildAt(countChild - 1);
		if (nodeChildTree.getUserObject() instanceof ForwardTreeUserObject) {
			return (ForwardTreeUserObject) nodeChildTree.getUserObject();
		}
		
		// Nodo con oggetto utente non conforme
		return null;
	}

	/**
	 * Get the next sibling node of the specified node.<br>
	 * <p>
	 * If the input node is the last of the children chain, a null value will be returned.<br>
	 * <p>
	 * @param nodeSibling as a user object that inherits from {@link ForwardTreeUserObject}
	 * @return the next sibling node as a user object that inherits from {@link ForwardTreeUserObject}
	 */
	public ForwardTreeUserObject _getNodeSiblingNext(ForwardTreeUserObject node) {
		
		DefaultMutableTreeNode nodeTree = null;
		DefaultMutableTreeNode nodeSibling = null;
		
		// Controllo di sicurezza 
		if (!(node instanceof ForwardTreeUserObject)) {return null;}
		
		// Recupero oggetto user nodo
		nodeTree = node.getNodeJTree();
		
		// Nodo fratello o null, se l'ultimo o nodo senza parent
		nodeSibling = nodeTree.getNextSibling();
		return (ForwardTreeUserObject) nodeSibling.getUserObject();
	}

	/**
	 * Get the child of a parent node at the specified index.<br>
	 * <p>
	 * If the index is out of bound, a null value will be returned.<br>
	 * <p>
	 * @param nodeParent as a user object that inherits from {@link ForwardTreeUserObject}
	 * @param index as the position, 0-based of the node to get
	 * @return the child node as a user object that inherits from {@link ForwardTreeUserObject}
	 */
	public ForwardTreeUserObject _getNodeChildAt(ForwardTreeUserObject nodeParent, int index) {
		
		ForwardTreeUserObject nodeParentUserObject = null;
		DefaultMutableTreeNode nodeParentTree = null;
		DefaultMutableTreeNode nodeChildTree = null;
		int countChild = 0;
		
		// Controllo di sicurezza 
		if (!(nodeParent instanceof ForwardTreeUserObject)) {return null;}
		
		// Recupero oggetto user e tree  nodo parent 
		nodeParentUserObject = (ForwardTreeUserObject) nodeParent;
		nodeParentTree = nodeParentUserObject.getNodeJTree();
		
		// No child: nessuna operazione
		countChild = getChildCount(nodeParentTree);
		if (index >= countChild) {return null;}
		
		// Oggetto corretto
		nodeChildTree = (DefaultMutableTreeNode) nodeParentTree.getChildAt(index);
		if (nodeChildTree.getUserObject() instanceof ForwardTreeUserObject) {
			return (ForwardTreeUserObject) nodeChildTree.getUserObject();
		}
		
		// Nodo con oggetto utente non conforme
		return null;
	}

	/**
	 * Get the child before of a nodeChild of a parent node.<br>
	 * <p>
	 * If there is no child before, a null value will be returned.<br>
	 * <p>
	 * @param nodeParent as a user object that inherits from {@link ForwardTreeUserObject}
	 * @return the child node as a user object that inherits from {@link ForwardTreeUserObject}
	 */
	public ForwardTreeUserObject _getNodeChildBefore(ForwardTreeUserObject nodeParent, ForwardTreeUserObject nodeChild) {
		
		ForwardTreeUserObject nodeParentUserObject = null;
		DefaultMutableTreeNode nodeParentTree = null;
		DefaultMutableTreeNode nodeChildTree = null;
		
		// Controllo di sicurezza 
		if (!(nodeParent instanceof ForwardTreeUserObject)) {return null;}
		
		// Recupero oggetto user e tree  nodo parent 
		nodeParentUserObject = (ForwardTreeUserObject) nodeParent;
		nodeParentTree = nodeParentUserObject.getNodeJTree();
		nodeChildTree = nodeChild.getNodeJTree();
		
		// Oggetto corretto
		nodeChildTree = (DefaultMutableTreeNode) nodeParentTree.getChildBefore(nodeChildTree);
		if (nodeChildTree.getUserObject() instanceof ForwardTreeUserObject) {
			return (ForwardTreeUserObject) nodeChildTree.getUserObject();
		}
		
		// Nodo con oggetto utente non conforme
		return null;
	}

	/**
	 * Get the parent node of a nodeChild.<br>
	 * <p>
	 * @param nodeChild as a user object that inherits from {@link ForwardTreeUserObject}
	 * @return the parent node as a user object that inherits from {@link ForwardTreeUserObject}
	 */
	public ForwardTreeUserObject _getNodeParent(ForwardTreeUserObject nodeChild) {
		
		DefaultMutableTreeNode nodeParentTree = null;
		DefaultMutableTreeNode nodeChildTree = null;
		
		// Controllo di sicurezza 
		if (!(nodeChild instanceof ForwardTreeUserObject)) {return null;}
		
		// Recupero oggetto user e tree  nodo parent 
		nodeChildTree = nodeChild.getNodeJTree();
		nodeParentTree = (DefaultMutableTreeNode) nodeChildTree.getParent();
		if (nodeParentTree.getUserObject() instanceof ForwardTreeUserObject) {
			return (ForwardTreeUserObject) nodeParentTree.getUserObject();
		}
		
		// Nodo con oggetto utente non conforme
		return null;
	}

	/**
	 * Get the child count of a parent node.<br>
	 * <p>
	 * @param nodeParent as a user object that inherits from {@link ForwardTreeUserObject}
	 */
	public int _getChildCount(ForwardTreeUserObject nodeParent) {
		DefaultMutableTreeNode nodeParentTree = null;
		int countChild = 0;
		
		// Controllo di sicurezza 
		if (!(nodeParent instanceof ForwardTreeUserObject)) {return 0;}
		
		// Recupero count child di nodo parent 
		nodeParentTree = nodeParent.getNodeJTree();
		countChild = getChildCount(nodeParentTree);
		return countChild;
	}

	/**
	 * Get the leaf count of a parent node.<br>
	 * <p>
	 * @param nodeParent as a user object that inherits from {@link ForwardTreeUserObject}
	 */
	public int _getLeafCount(ForwardTreeUserObject nodeParent) {
		DefaultMutableTreeNode nodeParentTree = null;
		int countLeaf = 0;
		
		// Controllo di sicurezza 
		if (!(nodeParent instanceof ForwardTreeUserObject)) {return 0;}
		
		// Recupero count child di nodo parent 
		nodeParentTree = nodeParent.getNodeJTree();
		countLeaf = nodeParentTree.getLeafCount();
		return countLeaf;
	}

	/**
	 * Get the index of the specified node child, under the parent node.<br>
	 * <p>
	 * @param nodeParent as a user object that inherits from {@link ForwardTreeUserObject}
	 * @param nodeChild as a user object that inherits from {@link ForwardTreeUserObject}
	 * @return the last child as a user object that inherits from {@link ForwardTreeUserObject}
	 */
	public int _getNodeChildIndex(ForwardTreeUserObject nodeChild) {
		
		DefaultMutableTreeNode nodeChildTree = null;
		int index = 0;
		
		// Controllo di sicurezza 
		if (!(nodeChild instanceof ForwardTreeUserObject)) {return 0;}
		
		nodeChildTree = nodeChild.getNodeJTree();
		index = nodeChildTree.getIndex(nodeChildTree);
		return index;
	}

	/**
	 * Expand all noodes in the tree starting from root
	 */
	public void _expandAll() {

	    // Traverse tree from root
		expandCollapsAll(this.jtree, new TreePath(rootNode.getNodeJTree()), true);
	}
	
	/**
	 * Expand all noodes under root at the first level
	 */
	public void _expandRoot() {
		TreePath rootPath = new TreePath(rootNode.getNodeJTree());
		this.jtree.expandPath(rootPath);
	}
	
	/**
	 * Collapse all noodes under root at the first level
	 */
	public void _collapseRoot() {
		TreePath rootPath = new TreePath(rootNode.getNodeJTree());
		this.jtree.collapsePath(rootPath);
	}
	
	/**
	 * Collaps all noodes in the tree starting from root
	 */
	public void _collapseAll() {
		expandCollapsAll(this.jtree, new TreePath(rootNode.getNodeJTree()), false);		// Traverse tree from root
	}
	
	@SuppressWarnings("rawtypes")
	private void expandCollapsAll(JTree tree, TreePath parent, boolean expand) {
	    // Traverse children
	    TreeNode node = (TreeNode)parent.getLastPathComponent();
	    if (node.getChildCount() >= 0) {
	        for (Enumeration e=node.children(); e.hasMoreElements(); ) {
	            TreeNode n = (TreeNode)e.nextElement();
	            TreePath path = parent.pathByAddingChild(n);
	            expandCollapsAll(tree, path, expand);
	        }
	    }

	    // Expansion or collapse must be done bottom-up
	    if (expand) {
	        tree.expandPath(parent);
	    } else {
	        tree.collapsePath(parent);
	    }
	}
 }
