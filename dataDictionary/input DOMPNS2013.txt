data dompns2013;
INFILE '...\dados\DOMPNS2013.txt' LRECL=142 missover;    
INPUT   
@0001   V0001     $2.   /*Unidade da Federação*/
@0003   V0024     $8.   /*Estrato*/
@0011   UPA_PNS   $7.   /*UPA*/
@0018   V0006_PNS $4.   /*Número de ordem do domicílio na PNS*/
@0022   V0015     $2.   /*Tipo de entrevista*/
@0024   V0020     $4.   /*Ano de referência*/
@0028   V0022     $2.   /*Total de moradores*/
@0030   V0026     $1.   /*Tipo de situação censitária*/
@0031   V0031     $1.   /*Tipo de área*/
@0032   V0028     14.8  /*Peso do domicílio sem calibração pela projeção de população*/
@0046   V0029     14.8  /*Peso do morador selecionado sem calibração pela projeção de população para morador selecionado*/
@0060   V00281    14.8  /*Peso do domicílio para cálculo de indicadores*/
@0074   V00291    14.8  /*Peso do morador selecionado para cálculo de indicadores*/
@0088   V00282     9.   /*Projeção da população*/
@0097   V00292    17.8  /*Projeção da população para morador selecionado*/
@0114   V00283    $3.   /*Domínio de pós-estrato 1*/
@0117   V00293    $5.   /*Domínio de pós-estrato 2*/
@0122   VDC001    $2.   /*Número de componentes do domicílio*/
@0124   VDC002    $2.   /*Total de moradores com 18 anos ou mais*/
@0126   UPA       $9.    /*Código da UPA das Pesquisas Domiciliares do Sistema Integrado*/
@0135   VDDATAD   $8.   /*Data de geração do arquivo*/
;
run;