package uu.jazy.core ;

/**
 * Lazy and Functional.
 * Package for laziness and functions as known from functional languages.
 * Written by Atze Dijkstra, atze@cs.uu.nl
 */

/**
 * An application of a Eval to 3 parameters.
 */
class Apply3 extends Apply
{
	//private static Stat statNew = Stat.newNewStat( "Apply3" ) ;
	
	protected Object p1, p2, p3 ;
	
	public Apply3( Object f, Object p1, Object p2, Object p3 )
	{
		super( f ) ;
		this.p1 = p1 ;
		this.p2 = p2 ;
		this.p3 = p3 ;
		//statNew.nrEvents++ ;
	}
	
    protected void eraseRefs()
    {
    	//function = null ;
    	p1 = p2 = p3 = null ;
    }
    
    public Object[] getBoundParams()
    {
	    if ( p1 == null )
	        return Utils.zeroArray ;
	    return new Object[] {p1,p2,p3} ;
    }

    public int getNrBoundParams()
    {
        return 3 ;
    }

}
