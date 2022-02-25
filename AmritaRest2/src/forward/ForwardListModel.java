package forward;
import java.io.Serializable;
import java.util.ArrayList;
import javax.swing.DefaultListModel;

/**
 * copyright (c) 2009-2012 e-Amrita - Ing Giampietro Zedda   Turin (Italy)
 * 
 * <h1>
 * ForwardListModel
 * </h1>
 * <p>
 * Holds additional data for a {@link JList} swing control useful for applications and completely user dependent.<br>
 * Often it's showed a list of items in the list but structured information must be maintened for each item,<br>
 * like a key or a numeric value application dependent.<br>
 * It's managed by this class an object for each item in the list and it's responsability of application manage the<br>
 * array of ojects when inserting, removing and updating items in the list.<br>
 * <p>
 * In any case item and objects bound must hold the same index.
 * <br>
 * <br>
 * Because this class inherits from {@link DefaultListModel}, all specific forward methods are starting with _ (underscore).<br>
 * Forward methods let to populate data bound to each list row.<br>
 * <p>
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 09/dic/2011 
 *
*/


public class ForwardListModel extends DefaultListModel implements Serializable {
	
	private static final long serialVersionUID = 1L;
    private ArrayList<Object> al_dataBound = null;				// One element for each list item
    private boolean isListDataBound = false;					// Binding active or not
    
    /** Empty Constructor */
	public ForwardListModel() {
		super();
		al_dataBound = new ArrayList<Object> ();
	}
	/**
	 * Get the structure twin of items list, with application objects bound.
	 * <br>
	 * @return the al_dataBound
	 */
	public ArrayList<Object> _getAllDataBound() {
		return al_dataBound;
	}
	
	/**
	 * Get the application object data bound to a list item.
	 * <br>
	 * 
	 * @param index the list row index 0-based
	 * @return the object bound to the row
	 */
	public Object _getDataBound(int index) {
		return al_dataBound.get(index);
	}


	/**
	 * Set the structure twin of items list, with application objects bound.
	 * <br>
	 * @param al_dataBound the al_dataBound to set
	 */
	public void _setAllDataBound(ArrayList<Object> al_dataBound) {
		this.isListDataBound = true;
		this.al_dataBound = al_dataBound;
	}



	/**
	 * Append an item to the list setting the bound object to null.
	 * <br>
	 * @param object the object to append to list
	 */
	public void _addElement(Object object) {
		addElement(object);
		al_dataBound.add(null);
		return;
	}

	/**
	 * Append an element to the list setting the bound object too.
	 * <br>
	 * @param object the object to append to the list
	 * @param objectBound the object to bind to the item
	 */
	public void _addElement(Object object, Object objectBound) {
		addElement(object);
		al_dataBound.add(objectBound);
		this.isListDataBound = true;
		return;
	}

	/**
	 * Insert an item to the list at the specified index setting the bound object to null.
	 * <br>
	 * @param object the object to append to list
	 * @param index the index position where to insert the item 0-based
	 */
	public void _insertElementAt(Object object, int index) {
		insertElementAt(object, index);
		al_dataBound.add(index, null);
		return;
	}

	/**
	 * Insert an item to the list at the specified index setting the bound object too.
	 * <br>
	 * @param object the object to append to the list
	 * @param objectBound the object to bind to the item
	 * @param index the index position where to insert the item 0-based
	 */
	public void _insertElementAt(Object object, Object objectBound, int index) {
		insertElementAt(object, index);
		al_dataBound.add(index, objectBound);
		this.isListDataBound = true;
		return;
	}

	/**
	 * Remove an item from the and the bound object too.
	 * <br>
	 * @param index the item index position 0-based 
	 */
	public void _remove(int index) {
		remove(index);
		al_dataBound.remove(index);
		return;
	}



	/**  
	 * Get if the list hold an object for any item.<br>
	 * <p>
	 * @return the isListDataBound
	 */
	public boolean _isListDataBound() {
		return isListDataBound;
	}
	
	/**
	 * Get if the list hold an object for any item.<br>
	 * <p>
	 * @param isListDataBound the isListDataBound to set
	 */
	public void _setListObjectBound(boolean isListDataBound) {
		this.isListDataBound = isListDataBound;
	}
}
