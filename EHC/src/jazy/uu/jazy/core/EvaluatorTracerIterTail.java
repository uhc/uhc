package uu.jazy.core ;/** * Lazy and Functional. * Package for laziness and functions as known from functional languages. * Written by Atze Dijkstra, atze@cs.uu.nl * * $Header:     $ * $Archive:    $ * $NoKeywords: $ *///import java.util.* ;//import java.io.* ;//import uu.jazy.prelude.* ;/** * Something which can be evaluated and/or applied to parameters. */public class EvaluatorTracerIterTail extends Evaluator{	private EvalTracer tracer ;		public EvaluatorTracerIterTail( EvalTracer t )	{		tracer = t ;	}	        /**     * Evaluate an Object known to be an Apply.     */    public Object eval( Object v )    {		if ( doTrace )		{			tracer.pushOnStack( v ) ;		}        try        {        	if ( v instanceof Apply )        	{	        	Apply v_prev = null ;		        do		        {		            Apply av = (Apply)v ;		        	if ( av.nrNeededParams == 0 )		        	{						av.evalSet() ;						av.nrNeededParams = -1 ;						nrEvaluations++ ;		            }		            else if ( av.nrNeededParams > 0 )		            {		                v = av ;		                break ;		            }					v = av.funcOrVal ;					if ( doTrace )					{						tracer.replaceTopOfStack( v ) ;					}					av.funcOrVal = v_prev ;					v_prev = av ;		        }		        while ( v instanceof Apply ) ;		        while ( v_prev != null )		        {		        	Apply av = (Apply)v_prev.funcOrVal ;		        	v_prev.funcOrVal = v ;		        	v_prev = av ;		        }	        }        }        catch ( Error err )        {            System.err.print( err.toString() + ", " ) ;            System.err.println( v.toString() ) ;            throw err ;        }        catch ( Throwable th )        {            //th.printStackTrace( System.err ) ;            System.err.print( th.toString() + ", " ) ;            System.err.println( v.toString() ) ;            //Utils.printCyclicOn( av, System.err, 3 ) ;            throw new Error( "Error in eval" ) ;        }        finally        {			if ( doTrace )			{				tracer.popFromStack() ;			}        }    	return v ;    }}