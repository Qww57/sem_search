%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Sentence exmaples with different complexity.
%
% Sentence ID: sXYZ is made so that X represents the complexity (number
% of recursion on NP) on the left side of the relation term and Y on
% the right side.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sentence(s000,[betacell,produce,insulin]).
sentence(s001,[betacell,isa,cell]).
sentence(s002,[betacell,produce,in,pancreas,insulin]).
sentence(s003,[insulin,is,produced,by,betacell]).
sentence(s004,[insulin,is,produced,in,pancreas,by,betacell]).
sentence(s005,[betacell,produce,in,'{',pancreas,in,body,'}'
	      ,insulin]).
sentence(s006,[betacell,produce,in,'{',pancreas,that,regulate,
	       body,'}',insulin]).
sentence(s007,[insulin,is,produced,in,'{',pancreas,in,body,'}',
	       by,betacell]).
sentence(s008,[insulin,is,produced,in,'{',pancreas,that,regulate,
	       body,'}',by,betacell]).
sentence(s009,[betacell,produce,in,pancreas,and,in,organ,and,in,
	       '{',body,that,regulate,hormone,'}',insulin]).
sentence(s010,[insulin,is,produced,in,pancreas,and,in,organ,and,in,
	       '{',body,that,regulate,hormone,'}',by,betacell]).
sentence(s011,[betacell,produce,'{',insulin, that,isa, hormone,'}']).
sentence(s012,[betacell,produce,'{',insulin, that,regulate,body,'}']).
sentence(s013,[betacell,produce,'{',insulin, in, pancreas,'}']).
sentence(s100,['{',betacell,in,organ, and, that,regulate,body,'}',
	       produce,insulin]).
sentence(s101,['{',betacell,that,produce,in,pancreas,insulin,'}',
	       isa,cell]).
sentence(s110,['{',betacell,that,regulate,body,'}',produce,'{',
	       insulin, that,regulate,body,'}']).
sentence(s111,['{',betacell,that,isa,cell,'}',produce,'{',
	       insulin,that,isa,hormone,'}']).
sentence(s112,['{',insulin,that,is,produced,by,betacell,'}',regulate,
	       body]).
sentence(s113,['{',insulin,that,is,produced,in,pancreas,by,betacell,
	       '}',regulate,body]).
sentence(s114,['{',insulin,that,is,produced,in,pancreas,by,betacell,
	       and,that,isa,hormone,and,that,regulate,body,'}',
	       regulate,body]).
sentence(s200,['{',betacell,in,organ,and,in,'{',pancreas,that,regulate,
	       body,'}','}',produce,insulin]).
sentence(s201,['{',betacell,that,produce,in,body,and,in,organ,and,in,
	       '{',pancreas,that,regulate,body,'}',insulin,'}',
	       produce,insulin]).
sentence(s202,['{',betacell,in,body,and,in,organ,and,in,'{',pancreas,
	       that,produce,insulin,'}','}',produce,insulin]).
sentence(s203,['{',cell,that,produce,'{',insulin,that,isa,hormone,'}',
	       '}',regulate,body]).
sentence(s210,['{',betacell,in,organ,and,in,body,and,in,'{',pancreas,
	       that,regulate,body,'}','}',produce,'{',insulin,that,
	       isa,hormone,'}']).
sentence(s310,['{',betacell,in,'{',pancreas,in,'{',body,that,produce,
	       insulin,'}','}','}',produce,'{',insulin,that,isa,
	       hormone,'}']).
sentence(s311,['{',betacell,that,isa,'{',cell,that,isa,organ,'}','}',
	       produce,insulin]).
sentence(s410,['{',betacell,in,'{',pancreas,that,isa,'{',organ,in,
	       body,and,that,produce, insulin,'}','}','}',
	       produce,'{',insulin,that,isa,hormone,'}']).




