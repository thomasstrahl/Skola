package expr;

import java.io.IOException;
import java.io.Reader;
import java.io.StreamTokenizer;
import java.io.StringReader;
import java.util.regex.Pattern;

import model.XLException;

/**
 * An <code>ExprParser</code> object is a parser provides a factory method for
 * building <code>Expr</code> objects from text representations of arithmetic
 * expressions. The text containing the expression should adhere to the
 * following grammar.
 * 
 * <pre>
 * 
 * 
 * 
 *    expr ::= term {addop term}
 *    term ::= factor {mulop factor}
 *    factor ::= number | variable | &quot;(&quot; expr &quot;)&quot;
 *    addop ::= &quot;+&quot; | &quot;-&quot;
 *    mulop ::= &quot;*&quot; | &quot;/&quot;
 * 
 * 
 * 
 * </pre>
 * 
 * where <code>number</code> is an unsigned number according to
 * <code>StreamTokenizer</code> and <code>variable</code> is a string of letters
 * and digits. The first character must be a letter.
 * 
 * @see Expr
 * @see StreamTokenizer
 * @author Lennart Andersson
 */
public class ExprParser {
    private int token;
    private StreamTokenizer tokenizer;

    /**
     * The <code>build</code> method returns an <code>Expr</code> representation
     * of the expression provided by <code>reader</code>.
     * 
     * @param reader
     *            a <code>Reader</code> provided the string to be parsed.
     * @return an <code>Expr</code> representation of the string.
     * @exception IOException
     *                if the <code>reader</code> does not deliver data.
     * @exception ExprParserException
     *                if the reader input violates the grammar.
     */
    public Expr build(Reader reader) throws IOException {
        tokenizer = new StreamTokenizer(reader);
        tokenizer.ordinaryChar('-');
        tokenizer.ordinaryChar('/');
        token = tokenizer.nextToken();
        Expr e = expr();
        if (token == StreamTokenizer.TT_EOF)
            return e;
        else
            throw new XLException("trailing garbage");
    }

    /**
     * The <code>build</code> method returns an <code>Expr</code> representation
     * of the expression provided by the <code>input</code> string.
     * 
     * @param input
     *            the <code>String</code> to be parsed.
     * @return an <code>Expr</code> representation of the string.
     * @exception IOException
     *                if the <code>input</code> does not deliver data.
     * @exception XLException
     *                if the input violates the grammar.
     */
    public Expr build(String input) throws IOException {
        return build(new StringReader(input));
    }

    private Expr expr() throws IOException {
        Expr result, term;
        result = term();
        while (token == '+' || token == '-') {
            int op = token;
            token = tokenizer.nextToken();
            term = term();
            switch (op) {
            case '+':
                result = new Add(result, term);
                break;
            case '-':
                result = new Sub(result, term);
                break;
            }
        }
        return result;
    }

    private Expr factor() throws IOException {
        Expr e;
        switch (token) {
        case '(':
            token = tokenizer.nextToken();
            e = expr();
            if (token != ')')
                throw new XLException("expecting \")\", found: " + token);
            token = tokenizer.nextToken();
            return e;
        case StreamTokenizer.TT_NUMBER:
            double x = tokenizer.nval;
            token = tokenizer.nextToken();
            return new Num(x);
        case StreamTokenizer.TT_WORD:
            String address = tokenizer.sval.toUpperCase();
            if (!Pattern.matches("[A-Z][0-9]+", address))
                throw new XLException("illegal address: " + address);
            token = tokenizer.nextToken();
            return new Variable(address);
        case StreamTokenizer.TT_EOF:
            throw new XLException("unexpected end of text");
        default:
            throw new XLException("unexpected " + (char) token);
        }
    }

    private Expr term() throws IOException {
        Expr result, factor;
        result = factor();
        while (token == '*' || token == '/') {
            int op = token;
            token = tokenizer.nextToken();
            factor = factor();
            switch (op) {
            case '*':
                result = new Mul(result, factor);
                break;
            case '/':
                result = new Div(result, factor);
                break;
            }
        }
        return result;
    }
}