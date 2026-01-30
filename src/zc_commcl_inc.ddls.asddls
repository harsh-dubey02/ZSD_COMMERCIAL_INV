@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection for Commercial Invoice'
@Metadata.allowExtensions: true
@UI.headerInfo:{
    typeName: 'Commercial Invoice',
    typeNamePlural: 'Commercial Invoice',
    title:{ type: #STANDARD, value: 'billingdocument' } }

define root view entity ZC_COMMCL_INC
  as projection on ZI_COMMCL_INC
{
      @UI.facet: [{ id : 'billingdocument',
       purpose: #STANDARD,
       type: #IDENTIFICATION_REFERENCE,
       label: 'Commercial Invoice',
        position: 10 }]
      @UI.lineItem:       [{ position: 10, label: 'billingdocument' },{ type: #FOR_ACTION , dataAction: 'ZPRINT', label: 'Generate Print'}]
      @UI.identification: [{ position: 10, label: 'billingdocument' }]
      @UI.selectionField: [{ position: 10 }]
  key BillingDocument,
      base64,
      m_ind
}
