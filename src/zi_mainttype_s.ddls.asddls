@EndUserText.label: 'Maint Type Singleton'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZI_MaintType_S
  as select from I_Language
    left outer join ZMAINT_TYPE on 0 = 0
  composition [0..*] of ZI_MaintType as _MaintType
{
  key 1 as SingletonID,
  _MaintType,
  max( ZMAINT_TYPE.LAST_CHANGED_AT ) as LastChangedAtMax,
  cast( '' as SXCO_TRANSPORT) as TransportRequestID,
  cast( 'X' as ABAP_BOOLEAN preserving type) as HideTransport
  
}
where I_Language.Language = $session.system_language
