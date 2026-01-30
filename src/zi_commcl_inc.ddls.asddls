@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'interface Commercial Invoice'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZI_COMMCL_INC
 as select from   I_BillingDocument as a
    left outer join ztb_commcl_inv   as b on a.BillingDocument = b.billingdocument 
{
  key a.BillingDocument,
      b.base64_3 as base64,
      b.m_ind
}
where a._DistributionChannel.DistributionChannel = '20'
and a.BillingDocumentType = 'F2';
