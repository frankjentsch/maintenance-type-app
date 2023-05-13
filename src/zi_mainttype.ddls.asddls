@EndUserText.label: 'Maintenance Type'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity ZI_MaintType
  as select from zmaint_type
  association to parent ZI_MaintType_S as _MaintTypeAll on $projection.SingletonID = _MaintTypeAll.SingletonID
{
  key maint_type_code as MaintTypeCode,
  ext_ref as ExternalReference,
  title as Title,
  description as Description,
  @Semantics.systemDateTime.lastChangedAt: true
  last_changed_at as LastChangedAt,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  local_last_changed_at as LocalLastChangedAt,
  1 as SingletonID,
  _MaintTypeAll
  
}
