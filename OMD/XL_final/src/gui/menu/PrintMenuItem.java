package gui.menu;

import gui.StatusLabel;
import gui.XL;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.PrintJob;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;
import javax.swing.JMenuItem;

import model.XLException;

class PrintMenuItem extends JMenuItem implements ActionListener {
    private StatusLabel statusLabel;
    private XL xl;

    public PrintMenuItem(XL xl, StatusLabel statusLabel) {
        super("Print");
        this.xl = xl;
        this.statusLabel = statusLabel;
        addActionListener(this);
    }

    public void actionPerformed(ActionEvent event) {
        PrinterJob printerJob = PrinterJob.getPrinterJob();
        printerJob.setPrintable(xl);
        // printJob.pageDialog(printJob.defaultPage());
        boolean doPrint = printerJob.printDialog();
        if (doPrint) {
            try {
                printerJob.print();
            } catch (PrinterException e) {
                statusLabel.setText(e.getMessage());
            }
        }
    }

    public void actionPerformed1(ActionEvent event) {
        Toolkit toolkit = xl.getToolkit();
        PrintJob printJob = toolkit.getPrintJob(xl, "Frame", null);
        if (printJob == null)
            throw new XLException("PrintJob failed");
        Graphics graphics = printJob.getGraphics();
        Dimension size = xl.getSize();
        Dimension pageSize = printJob.getPageDimension();
        graphics.translate((pageSize.width - size.width) / 2,
                (pageSize.height - size.height) / 2);
        graphics.drawRect(-1, -1, size.width + 1, size.height + 1);
        xl.printAll(graphics);
        graphics.dispose();
        printJob.end();
    }
}