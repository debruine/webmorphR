/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package plugins;

import Facemorph.Template;
import Facemorph.psychomorph.Batchable;
import Facemorph.psychomorph.ImageZoomPanel;
import Facemorph.psychomorph.PsychoMorphForm;
import java.awt.Image;
import java.awt.geom.Point2D;

/**
 * Implements adding extra points to the "old" standard template in the style of Ben and Lisa (mostly around the neck, nose and ears)
 * The points should be correct afterwards.
 * @author bpt
 */
public class BatchExtraPoints implements Batchable {

    PsychoMorphForm psychomorph;

    @Override
    public boolean process(ImageZoomPanel izp, boolean single) {
        Template tem = izp.getTemplate();
         if (single) {
             Image img = izp.getImage();
        psychomorph.getDelineator().getImageUndoStack().push(img);
        psychomorph.getDelineator().getTemplateUndoStack().push(tem.clone());
        psychomorph.getDelineator().getTransformUndoMenuItem().setEnabled(true);
    }

        if (tem.getPoints().size()!=179 || tem.getContours().size()!=39) {
            //JOptionPane.showMessageDialog(psychomorph.getDelineator(), "");
            return false;
        }
        Point2D.Float p1 = tem.getPoint(65);
        Point2D.Float p2 = tem.getPoint(55);
        Point2D.Float p3 = tem.getPoint(64);

        tem.addContourPoint(14, p1.x-(p3.x-p2.x)/4f, p1.y-(p3.y-p2.y)/4f);
        tem.addContourPoint(14, p2.x-(p2.x-p3.x)/4f, p2.y-(p2.y-p3.y)/4f);

         p1 = tem.getPoint(70);
         p3 = tem.getPoint(69);
       // p2 = tem.getPoint(55);

         tem.addContourPoint(15, p2.x-(p2.x-p3.x)/4f, p2.y-(p2.y-p3.y)/4f);//p1.x-2*(p1.x-p2.x)/3f, p2.y);
        tem.addContourPoint(15, p1.x-(p3.x-p2.x)/4f, p1.y-(p3.y-p2.y)/4f);//p1.x-(p1.x-p2.x)/3f, p1.y);
        
        p1 = tem.getPoint(127);
        p2 = tem.getPoint(128);
       // p3 = tem.getPoint(129);
        Point2D.Float mid = new Point2D.Float((p1.x+p2.x)/2f, (p1.y+p2.y)/2f);
        tem.addPoint(mid.x-(p2.y-p1.y)/1f, mid.y+(p2.x-p1.x)/1f);//p1.x-(p1.x-p2.x)/3f, p3.y+(p2.y-p1.y)/3f);

        p1 = tem.getPoint(131);
        p2 = tem.getPoint(130);
        mid = new Point2D.Float((p1.x+p2.x)/2f, (p1.y+p2.y)/2f);
        tem.addPoint(mid.x+(p2.y-p1.y)/1f, mid.y-(p2.x-p1.x)/1f);//p1.x-(p1.x-p2.x)/3f, p3.y+(p2.y-p1.y)/3f);

        p1 = tem.getPoint(109);
        p2 = tem.getPoint(110);
        tem.addContourPoint(24, (p1.x+p2.x)/2f, (p2.y+p1.y)/2f);

        p1 = tem.getPoint(111);
        p2 = tem.getPoint(110);
        tem.addContourPoint(24, (p1.x+p2.x)/2f, (p2.y+p1.y)/2f);

        p1 = tem.getPoint(112);
        p2 = tem.getPoint(113);
        tem.addContourPoint(25, (p1.x+p2.x)/2f, (p2.y+p1.y)/2f);

        p1 = tem.getPoint(113);
        p2 = tem.getPoint(114);
        tem.addContourPoint(25, (p1.x+p2.x)/2f, (p2.y+p1.y)/2f);


        tem.deleteLine(28);
        
        tem.addContour(126, false);
        tem.addContourPoint(145);

        tem.addContour(132, false);
        tem.addContourPoint(157);

        tem.addContour(145, false);
        tem.addContourPoint(183);
        tem.addContourPoint(184);
        tem.addContourPoint(157);

        tem.addContour(111, false);
        tem.addContourPoint(125);
        tem.addContourPoint(126);

        tem.addContour(126, false);
        tem.addContourPoint(127);
        tem.addContourPoint(128);
        tem.addContourPoint(129);
        tem.addContourPoint(130);
        tem.addContourPoint(131);
        tem.addContourPoint(132);

        tem.addContour(132, false);
        tem.addContourPoint(133);
        tem.addContourPoint(114);
        izp.setTemplate(tem);
        return true;
    }

    @Override
    public boolean initialise(PsychoMorphForm psychomorph) {
    this.psychomorph = psychomorph;
    return true;
    }

    @Override
    public void finish() {

    }

    @Override
    public String getName() {
        return "Old to New Template";
    }

    public boolean getReadTemplate() {
        return true;
    }

    public boolean getWriteTemplate() {
        return true;
    }

    public boolean getWriteImage() {
        return false;
    }

   
}
