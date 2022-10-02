/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package plugins;

import Facemorph.Template;
import Facemorph.psychomorph.Batchable;
import Facemorph.psychomorph.ImageZoomPanel;
import Facemorph.psychomorph.PsychoMorphForm;
import java.awt.geom.Point2D;

/**
 * Batch corrects the mouth in the "standard" template, by checking if the top -lip is below the bottom lip points and swapping if needed
 * @author bpt
 */
public class BatchCorrectMouth implements Batchable {

    @Override
    public boolean process(ImageZoomPanel izp, boolean single) {
       Template tem = izp.getTemplate();
       int[] topMouth = {94, 95, 96, 97, 98}, bottomMouth = {99, 100, 101, 102, 103};

       for (int i=0; i<topMouth.length; i++) {
            Point2D.Float p = tem.getPoint(topMouth[i]);
            Point2D.Float q = tem.getPoint(bottomMouth[i]);
            if (q.y<p.y) {
                float x = p.x, y=p.y;
                p.x=q.x; p.y=q.y;
                q.x=x; q.y=y;
            }
       }
       tem.recalculateContours();
       izp.setTemplate(tem);
       return true;
    }

    @Override
    public void finish() {

    }

    @Override
    public String getName() {
        return "Correct Mouth";
    }

    @Override
    public boolean initialise(PsychoMorphForm psychomorph) {
      return true;//  throw new UnsupportedOperationException("Not supported yet.");
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
