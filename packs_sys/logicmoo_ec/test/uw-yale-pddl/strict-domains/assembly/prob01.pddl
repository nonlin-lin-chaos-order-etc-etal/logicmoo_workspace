(DEFINE (PROBLEM ASSEMBLY-TEST)
   (:DOMAIN ASSEMBLY)
   (:OBJECTS LWING RWING FUSELAGE TAIL LFLAPS RFLAPS ENGINE1 ENGINE2
             PROPELLER1 PROPELLER2 LINCHPIN AIRPLANE - ASSEMBLY
             SCAFFOLD - RESOURCE)
   (:INIT (AVAILABLE SCAFFOLD)
          (AVAILABLE LINCHPIN)
          (AVAILABLE PROPELLER1)
          (AVAILABLE PROPELLER2)
          (AVAILABLE LFLAPS)
          (AVAILABLE RFLAPS)
          (AVAILABLE FUSELAGE)
          (REQUIRES LWING SCAFFOLD)
          (REQUIRES RWING SCAFFOLD)
          (REQUIRES TAIL SCAFFOLD)
          (REQUIRES AIRPLANE SCAFFOLD)
          (TRANSIENT-PART LINCHPIN ENGINE1)
          (TRANSIENT-PART LINCHPIN ENGINE2)
          (PART-OF PROPELLER1 ENGINE1)
          (PART-OF PROPELLER2 ENGINE2)
          (PART-OF ENGINE1 LWING)
          (PART-OF ENGINE2 RWING)
          (PART-OF LFLAPS LWING)
          (PART-OF RFLAPS RWING)
          (PART-OF LWING AIRPLANE)
          (PART-OF RWING AIRPLANE)
          (PART-OF FUSELAGE AIRPLANE)
          (PART-OF LINCHPIN TAIL)
          (PART-OF TAIL AIRPLANE)
          (ASSEMBLE-ORDER LINCHPIN PROPELLER1 ENGINE1)
          (ASSEMBLE-ORDER LINCHPIN PROPELLER2 ENGINE2)
          (REMOVE-ORDER PROPELLER1 LINCHPIN ENGINE1)
          (REMOVE-ORDER PROPELLER2 LINCHPIN ENGINE2)
          (ASSEMBLE-ORDER FUSELAGE LWING AIRPLANE)
          (ASSEMBLE-ORDER FUSELAGE RWING AIRPLANE)
          (ASSEMBLE-ORDER FUSELAGE TAIL AIRPLANE))
   (:GOAL (COMPLETE AIRPLANE)))