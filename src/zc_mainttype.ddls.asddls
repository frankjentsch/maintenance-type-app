@EndUserText.label: 'Maintenance Type - Maintain'
@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
define view entity ZC_MaintType
  as projection on ZI_MaintType
{
  key MaintTypeCode,
  ExternalReference,
  Title,
  Description,
  LastChangedAt,
  @Consumption.hidden: true
  LocalLastChangedAt,
  @Consumption.hidden: true
  SingletonID,
  _MaintTypeAll : redirected to parent ZC_MaintType_S
  
}
