data dompns2013;
INFILE '...\dados\DOMPNS2013.txt' LRECL=142 missover;    
INPUT   
@0001   V0001     $2.   /*Unidade da Federa��o*/
@0003   V0024     $8.   /*Estrato*/
@0011   UPA_PNS   $7.   /*UPA*/
@0018   V0006_PNS $4.   /*N�mero de ordem do domic�lio na PNS*/
@0022   V0015     $2.   /*Tipo de entrevista*/
@0024   V0020     $4.   /*Ano de refer�ncia*/
@0028   V0022     $2.   /*Total de moradores*/
@0030   V0026     $1.   /*Tipo de situa��o censit�ria*/
@0031   V0031     $1.   /*Tipo de �rea*/
@0032   V0028     14.8  /*Peso do domic�lio sem calibra��o pela proje��o de popula��o*/
@0046   V0029     14.8  /*Peso do morador selecionado sem calibra��o pela proje��o de popula��o para morador selecionado*/
@0060   V00281    14.8  /*Peso do domic�lio para c�lculo de indicadores*/
@0074   V00291    14.8  /*Peso do morador selecionado para c�lculo de indicadores*/
@0088   V00282     9.   /*Proje��o da popula��o*/
@0097   V00292    17.8  /*Proje��o da popula��o para morador selecionado*/
@0114   V00283    $3.   /*Dom�nio de p�s-estrato 1*/
@0117   V00293    $5.   /*Dom�nio de p�s-estrato 2*/
@0122   VDC001    $2.   /*N�mero de componentes do domic�lio*/
@0124   VDC002    $2.   /*Total de moradores com 18 anos ou mais*/
@0126   UPA       $9.    /*C�digo da UPA das Pesquisas Domiciliares do Sistema Integrado*/
@0135   VDDATAD   $8.   /*Data de gera��o do arquivo*/
;
run;