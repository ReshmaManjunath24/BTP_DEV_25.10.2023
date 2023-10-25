@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Data Definition for CLAP Users Information'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZDD_CLAP_USER_INFO as select from I_BusinessUserVH
{

    key BusinessPartner,
    BPIdentificationNumber,
    UserID,
    FirstName,
    LastName,
    DefaultEmailAddress,
    PersonFullName,
    Building,
    RoomNumber,
    Department,
    IsBusinessPurposeCompleted,
    AuthorizationGroup
}
    
