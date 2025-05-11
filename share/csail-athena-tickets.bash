# to be included from ~/.bashrc

afs4athena() {

	function help()	
	{ #yes this is nasty
	  echo
	  echo "this function will get CSAIL and ATHENA krb5 tickets
	  and afs tokens, the default krb5 principal will be your
	  CSAIL id in /tmp/krb5cc_$UID, the ATHENA tickets will be in
	  /tmp/krb5cc_$UID.athena"
	  echo
	  echo
	  echo 'usage: afs4athena [athena_username]'
	  echo
	  echo 'if athena_username is not specified it defaults to $USER'
	  echo
	}

	function get_tokens()
	{
	
	  KRB5CCNAME=/tmp/krb5cc_$UID 
	  kinit -5 $USER@CSAIL.MIT.EDU
	  aklog -cell csail.mit.edu

	  KRB5CCNAME=/tmp/krb5cc_$UID.athena 
	  kinit -5 $ATHENA_NAME@ATHENA.MIT.EDU
	  aklog -cell athena

	  KRB5CCNAME=/tmp/krb5cc_$UID 
	
	}

case $1 in 
     ( "" ) ATHENA_NAME=$USER 
	    get_tokens ;;

     ( -h* | -H*) help;;

     ( * )  ATHENA_NAME=$1 
	    get_tokens;;
esac

}
