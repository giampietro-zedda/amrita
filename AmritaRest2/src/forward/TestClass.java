package forward;

import java.awt.BorderLayout;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

public class TestClass {
  public static void main(String args[]) throws Exception {
    JFrame frame = new JFrame("TextPane Example");
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    StyleContext context = new StyleContext();
    StyledDocument document = new DefaultStyledDocument(context);

    Style style = context.getStyle(StyleContext.DEFAULT_STYLE);
    StyleConstants.setAlignment(style, StyleConstants.ALIGN_LEFT);
    StyleConstants.setFontSize(style, 14);
    StyleConstants.setSpaceAbove(style, 4);
    StyleConstants.setSpaceBelow(style, 4);

    SimpleAttributeSet attributesSet = new SimpleAttributeSet();
    StyleConstants.setBold(attributesSet, true);
    StyleConstants.setItalic(attributesSet, true);
    StyleConstants.setFontSize(attributesSet, 24);

    // Third style for icon/component
    Style labelStyle = context.getStyle(StyleContext.DEFAULT_STYLE);

    Icon icon = new ImageIcon("Computer.gif");
    JLabel label = new JLabel(icon);
    StyleConstants.setComponent(labelStyle, label);

    try {
      document.insertString(document.getLength(), "Hello www.java2s.com\n", attributesSet);
      document.insertString(document.getLength(), "Middle", style);
      document.insertString(document.getLength(), "Hello www.java2s.com\n", attributesSet);
     document.insertString(document.getLength(), "Ignored", labelStyle);
    } catch (BadLocationException badLocationException) {
      System.err.println("Oops");
    }

    JTextPane textPane = new JTextPane(document);
    textPane.setEditable(true);
    JScrollPane scrollPane = new JScrollPane(textPane);
    frame.add(scrollPane, BorderLayout.CENTER);

    frame.setSize(300, 150);
    frame.setVisible(true);
  }
}