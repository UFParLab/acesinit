#ifndef _ECP_INFO_
#define _ECP_INFO_
C
C Basic parameters: Maxang set to 7 (i functions) and Maxproj set
C 5 (up to h functions in projection space).

      Integer Maxang, Maxproj, Lmxecp, Maxjco
      Integer Mxecpprim, max_sub_shells, Ipseud

      Parameter(Maxang=7, Maxproj=6, Lmxecp=7,
     &          Mxecpprim=Max_prims*Max_centers_dupl, Maxjco=10)

      Double Precision clp(Mxecpprim), zlp(Mxecpprim)
      Integer nlp(Mxecpprim), kfirst(Maxang,Max_centers_dupl)
      Integer klast(Maxang,Max_centers_dupl), llmax(Max_centers_dupl)
      Integer nelecp(Max_centers_dupl), ipseux(Max_centers_dupl) 
      Integer iqmstr(max_centers_dupl), jcostr(max_centers_dupl,maxjco)
      Integer nucstr(max_centers_dupl, maxshells, maxjco)
      Integer nrcstr(max_centers_dupl, maxshells, maxjco)
      Integer jstrt(maxshells), jrs(maxshells)

      common/ECP_POT_VARS/clp, zlp, nlp, kfirst, klast, llmax
      common /pseud / nelecp, ipseux, ipseud
      common /ECP_VAL_BASIS_VARS/iqmstr, jcostr, nucstr,
     &                           nrcstr, jstrt, jrs, max_sub_shells

#endif /* _ECP_INFO_ */

