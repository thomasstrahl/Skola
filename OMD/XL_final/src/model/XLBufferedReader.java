package model;

import gui.SlotLabels;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;



//TODO move to another package
public class XLBufferedReader extends BufferedReader {
    public XLBufferedReader(String name) throws FileNotFoundException {
        super(new FileReader(name));
    }

    // TODO Change Object to something appropriate
    public void load(Sheet sheet, SlotLabels labels) {
    
        try {
        	Sheet tempSheet = new Sheet(labels);
        	tempSheet.setLoad(true);
        	while (ready()) {
                String string = readLine();
                int i = string.indexOf('=');
                tempSheet.updateField(string.substring(0, i), string.substring(i+1, string.length()));
        	}
        	tempSheet.setLoad(false);
        	tempSheet.checkErrors();
        	sheet.merge(tempSheet); 
       
        	} catch (Exception e) {
        		throw new XLException(e.getMessage());
        	}
    	}
}
