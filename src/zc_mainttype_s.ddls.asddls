@EndUserText.label: 'Maint Type Singleton - Maintain'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@ObjectModel.semanticKey: [ 'SingletonID' ]
define root view entity ZC_MaintType_S
  provider contract transactional_query
  as projection on ZI_MaintType_S
{
  key SingletonID,
  LastChangedAtMax,
  TransportRequestID,
  HideTransport,
  _MaintType : redirected to composition child ZC_MaintType
  
}
