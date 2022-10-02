/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package plugins;

import Facemorph.psychomorph.Plugin;
import Facemorph.psychomorph.PsychoMorphForm;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;

/**
 *
 * @author bpt
 */
public class AddMenuTest  extends JMenu implements Plugin{
    PsychoMorphForm psychomorph;
    

    @Override
    public boolean setup(PsychoMorphForm psychomorph) {
         this.psychomorph = psychomorph;
        JMenuItem anItem = new JMenuItem("Boo!");
        anItem.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                JOptionPane.showMessageDialog(AddMenuTest.this.psychomorph, "Boo!");

            }

        });
        this.setText("Boo!");
        System.out.println("setup, menu = " + System.identityHashCode(this));

        this.add(anItem);

        psychomorph.addMenu(this);

        return true;//throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public boolean cleanup(PsychoMorphForm psychomorph) {
        System.out.println("cleanup, menu = " + System.identityHashCode(this));
        psychomorph.removeMenu(this);
        return true;//throw new UnsupportedOperationException("Not supported yet.");
    }
}
