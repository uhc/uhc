%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: plan
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A Plan holds together all ingredients for a memory management strategy.
There can only be one Plan for a running program, configured statically
and/or partially configured at runtime

%%[8
#include "../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Default plan
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Plan mm_plan ;

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_init_plan() {

#	if (MM_Cfg_Plan == MM_Cfg_Plan_SS)
		mm_plan = mm_plan_SS ;
#	endif
	mm_plan.init( &mm_plan ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Plan test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_plan_Test() {
#	if (MM_Cfg_Plan == MM_Cfg_Plan_SS)
		mm_plan.dump( &mm_plan ) ;
		GB_NodePtr p1 = (GB_NodePtr)mm_plan.mutator->allocator->alloc( mm_plan.mutator->allocator, 12 ) ;
		IF_GB_TR_ON(3,{printf("mm_plan_Test alloc p1=%x\n",p1);}) ;
		GB_FillConNode2(p1,2,p1,p1)
		mm_Roots_Register( (Ptr)&p1 ) ;
		
		int i1 ;
		for ( i1 = 0 ; i1 < 500 ; i1++ ) {
			int i2 ;
			for ( i2 = 0 ; i2 < 100 ; i2++ ) {
				Ptr p = mm_plan.mutator->allocator->alloc( mm_plan.mutator->allocator, 24 ) ;
				IF_GB_TR_ON(3,{printf("mm_plan_Test %d %d alloc p=%x\n",i1,i2,p);}) ;
			}
			mm_plan.dump( &mm_plan ) ;
		}
#	endif
}
#endif
%%]

