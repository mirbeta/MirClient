{*******************************************************}
{               MiTeC Common Routines                   }
{                     LSA API                           }
{                                                       }
{         Copyright (c) 1997-2014 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_LSAAPI;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows;
     {$ELSE}
     Windows;
     {$ENDIF}

// The flags in the security operational mode are defined
// as:
//
//    PasswordProtected - Some level of authentication (such as
//        a password) must be provided by users before they are
//        allowed to use the system.  Once set, this value will
//        not be cleared without re-booting the system.
//
//    IndividualAccounts - Each user must identify an account to
//        logon to.  This flag is only meaningful if the
//        PasswordProtected flag is also set.  If this flag is
//        not set and the PasswordProtected flag is set, then all
//        users may logon to the same account.  Once set, this value
//        will not be cleared without re-booting the system.
//
//    MandatoryAccess - Indicates the system is running in a mandatory
//        access control mode (e.g., B-level as defined by the U.S.A's
//        Department of Defense's "Orange Book").  This is not utilized
//        in the current release of NT.  This flag is only meaningful
//        if both the PasswordProtected and IndividualAccounts flags are
//        set.  Once set, this value will not be cleared without
//        re-booting the system.
//
//    LogFull - Indicates the system has been brought up in a mode in
//        which if must perform security auditing, but its audit log
//        is full.  This may (should) restrict the operations that
//        can occur until the audit log is made not-full again.  THIS
//        VALUE MAY BE CLEARED WHILE THE SYSTEM IS RUNNING (I.E., WITHOUT
//        REBOOTING).
//
// If the PasswordProtected flag is not set, then the system is running
// without security, and user interface should be adjusted appropriately.
//

const
 STATUS_SUCCESS = 0;
 LSA_MODE_PASSWORD_PROTECTED     = $00000001;
 LSA_MODE_INDIVIDUAL_ACCOUNTS    = $00000002;
 LSA_MODE_MANDATORY_ACCESS       = $00000004;
 LSA_MODE_LOG_FULL               = $00000008;

//
// The following defines describe the auditing options for each
// event type
//

 POLICY_AUDIT_EVENT_UNCHANGED       = $00000000;  // Leave options specified for this event unchanged
 POLICY_AUDIT_EVENT_SUCCESS         = $00000001;  // Audit successful occurrences of events of this type
 POLICY_AUDIT_EVENT_FAILURE         = $00000002;  // Audit failed attempts to cause an event of this type to occur
 POLICY_AUDIT_EVENT_NONE            = $00000004;

// Mask of valid event auditing options

 POLICY_AUDIT_EVENT_MASK =
   POLICY_AUDIT_EVENT_SUCCESS or
   POLICY_AUDIT_EVENT_FAILURE or
   POLICY_AUDIT_EVENT_UNCHANGED or
   POLICY_AUDIT_EVENT_NONE;

//
// Access types for the Policy object
//

 POLICY_VIEW_LOCAL_INFORMATION              = $00000001;
 POLICY_VIEW_AUDIT_INFORMATION              = $00000002;
 POLICY_GET_PRIVATE_INFORMATION             = $00000004;
 POLICY_TRUST_ADMIN                         = $00000008;
 POLICY_CREATE_ACCOUNT                      = $00000010;
 POLICY_CREATE_SECRET                       = $00000020;
 POLICY_CREATE_PRIVILEGE                    = $00000040;
 POLICY_SET_DEFAULT_QUOTA_LIMITS            = $00000080;
 POLICY_SET_AUDIT_REQUIREMENTS              = $00000100;
 POLICY_AUDIT_LOG_ADMIN                     = $00000200;
 POLICY_SERVER_ADMIN                        = $00000400;
 POLICY_LOOKUP_NAMES                        = $00000800;

 POLICY_ALL_ACCESS     = STANDARD_RIGHTS_REQUIRED         or
                         POLICY_VIEW_LOCAL_INFORMATION    or
                         POLICY_VIEW_AUDIT_INFORMATION    or
                         POLICY_GET_PRIVATE_INFORMATION   or
                         POLICY_TRUST_ADMIN               or
                         POLICY_CREATE_ACCOUNT            or
                         POLICY_CREATE_SECRET             or
                         POLICY_CREATE_PRIVILEGE          or
                         POLICY_SET_DEFAULT_QUOTA_LIMITS  or
                         POLICY_SET_AUDIT_REQUIREMENTS    or
                         POLICY_AUDIT_LOG_ADMIN           or
                         POLICY_SERVER_ADMIN              or
                         POLICY_LOOKUP_NAMES;


 POLICY_READ           = STANDARD_RIGHTS_READ             or
                         POLICY_VIEW_AUDIT_INFORMATION    or
                         POLICY_GET_PRIVATE_INFORMATION;

 POLICY_WRITE          = STANDARD_RIGHTS_WRITE            or
                         POLICY_TRUST_ADMIN               or
                         POLICY_CREATE_ACCOUNT            or
                         POLICY_CREATE_SECRET             or
                         POLICY_CREATE_PRIVILEGE          or
                         POLICY_SET_DEFAULT_QUOTA_LIMITS  or
                         POLICY_SET_AUDIT_REQUIREMENTS    or
                         POLICY_AUDIT_LOG_ADMIN           or
                         POLICY_SERVER_ADMIN;

 POLICY_EXECUTE        = STANDARD_RIGHTS_EXECUTE          or
                         POLICY_VIEW_LOCAL_INFORMATION    or
                         POLICY_LOOKUP_NAMES;
 TOKEN_SOURCE_LENGTH   = 8;
           
 SE_INTERACTIVE_LOGON_NAME         = 'SeInteractiveLogonRight';
 SE_NETWORK_LOGON_NAME             = 'SeNetworkLogonRight';
 SE_BATCH_LOGON_NAME               = 'SeBatchLogonRight';
 SE_SERVICE_LOGON_NAME             = 'SeServiceLogonRight';
 SE_CREATE_TOKEN_NAME              = 'SeCreateTokenPrivilege';
 SE_ASSIGNPRIMARYTOKEN_NAME        = 'SeAssignPrimaryTokenPrivilege';
 SE_LOCK_MEMORY_NAME               = 'SeLockMemoryPrivilege';
 SE_INCREASE_QUOTA_NAME            = 'SeIncreaseQuotaPrivilege';
 SE_UNSOLICITED_INPUT_NAME         = 'SeUnsolicitedInputPrivilege';
 SE_MACHINE_ACCOUNT_NAME           = 'SeMachineAccountPrivilege';
 SE_TCB_NAME                       = 'SeTcbPrivilege';
 SE_SECURITY_NAME                  = 'SeSecurityPrivilege';
 SE_TAKE_OWNERSHIP_NAME            = 'SeTakeOwnershipPrivilege';
 SE_LOAD_DRIVER_NAME               = 'SeLoadDriverPrivilege';
 SE_SYSTEM_PROFILE_NAME            = 'SeSystemProfilePrivilege';
 SE_SYSTEMTIME_NAME                = 'SeSystemtimePrivilege';
 SE_PROF_SINGLE_PROCESS_NAME       = 'SeProfileSingleProcessPrivilege';
 SE_INC_BASE_PRIORITY_NAME         = 'SeIncreaseBasePriorityPrivilege';
 SE_CREATE_PAGEFILE_NAME           = 'SeCreatePagefilePrivilege';
 SE_CREATE_PERMANENT_NAME          = 'SeCreatePermanentPrivilege';
 SE_BACKUP_NAME                    = 'SeBackupPrivilege';
 SE_RESTORE_NAME                   = 'SeRestorePrivilege';
 SE_SHUTDOWN_NAME                  = 'SeShutdownPrivilege';
 SE_DEBUG_NAME                     = 'SeDebugPrivilege';
 SE_AUDIT_NAME                     = 'SeAuditPrivilege';
 SE_SYSTEM_ENVIRONMENT_NAME        = 'SeSystemEnvironmentPrivilege';
 SE_CHANGE_NOTIFY_NAME             = 'SeChangeNotifyPrivilege';
 SE_REMOTE_SHUTDOWN_NAME           = 'SeRemoteShutdownPrivilege';

 NoPrivileges = 28;
type
 NTStatus = DWORD;
 TQuotaLimits = packed record
   PagedPoolLimit : DWORD;
   NonPagedPoolLimit : DWORD;
   MinimumWorkingSetSize : DWORD;
   MaximumWorkingSetSize : DWORD;
   PagefileLimit : DWORD;
   TimeLimit : TLargeInteger
 end;
 PQuotaLimits = ^TQuotaLimits;

 TTokenSource = packed record
   Sourcename : array [0..TOKEN_SOURCE_LENGTH - 1] of char;
   SourceIdentifier : TLargeInteger
 end;
 PTokenSource = ^TTokenSource;


 LSA_OPERATIONAL_MODE = ULONG;
 PLSA_OPERATIONAL_MODE = ^LSA_OPERATIONAL_MODE;

// Used by a logon process to indicate what type of logon is being
// requested.
// NOTE: Proxy logon type is not currently supported in NT 3.x

 TSecurityLogonType = (d1, d2,
   Interactive,
   Network,
   Batch,
   Service,
   Proxy,
   Unlock);

 PSecurityLogonType = ^TSecurityLogonType;

//
// Audit Event Categories
//
// The following are the built-in types or Categories of audit event.
// WARNING!  This structure is subject to expansion.  The user should not
// compute the number of elements of this type directly, but instead
// should obtain the count of elements by calling LsaQueryInformationPolicy()
// for the PolicyAuditEventsInformation class and extracting the count from
// the MaximumAuditEventCount field of the returned structure.
//

 TPolicyAuditEventType = (
   AuditCategorySystem,
   AuditCategoryLogon,
   AuditCategoryObjectAccess,
   AuditCategoryPrivilegeUse,
   AuditCategoryDetailedTracking,
   AuditCategoryPolicyChange,
   AuditCategoryAccountManagement
 );
 PPolicyAuditEventType = ^TPolicyAuditEventType;

 TLSAUnicodeString = packed record
   Length : WORD;
   MaximumLength : WORD;
   Buffer : PWideChar
 end;
 PLSAUnicodeString = ^TLSAUnicodeString;

 TLSAString = packed record
   Length : WORD;
   MaximumLength : WORD;
   Buffer : PAnsiChar;
 end;
 PLSAString = ^TLSAString;

 TLSAObjectAttributes = packed record
   Length : ULONG;
   RootDirectory : THandle;
   ObjectName : PLSAUnicodeString;
   Attributes : ULONG;
   SecurityDescriptor : pointer;        // Points to type SECURITY_DESCRIPTOR
   SecurityQualityOfService : pointer;  // Points to type SECURITY_QUALITY_OF_SERVICE
 end;
 PLSAObjectAttributes = ^TLSAObjectAttributes;

//
// Policy object specific data types.
//

//
// The following data type is used to identify a domain
//

 TLsaTrustInformation = packed record
   Name : TLsaUnicodeString;
   Sid : PSid;
 end;
 PLsaTrustInformation = ^TLsaTrustInformation;

// where members have the following usage:
//
//     Name - The name of the domain.
//
//     Sid - A pointer to the Sid of the Domain
//

//
// The following data type is used in name and SID lookup services to
// describe the domains referenced in the lookup operation.
//

 TLsaReferencedDomainList = packed record
   Entries : ULONG;
   Domains : PLsaTrustInformation
 end;
 PLsaReferencedDomainList = ^TLsaReferencedDomainList;

// where members have the following usage:
//
//     Entries - Is a count of the number of domains described in the
//         Domains array.
//
//     Domains - Is a pointer to an array of Entries LSA_TRUST_INFORMATION data
//         structures.
//


//
// The following data type is used in name to SID lookup services to describe
// the domains referenced in the lookup operation.
//

 TLsaTranslatedSID = packed record
   use : SID_NAME_USE;
   RelativeId : ULONG;
   DomainIndex : LongInt
 end;
 PLsaTranslatedSID = ^TLsaTranslatedSID;

// where members have the following usage:
//
//     Use - identifies the use of the SID.  If this value is SidUnknown or
//         SidInvalid, then the remainder of the record is not set and
//         should be ignored.
//
//     RelativeId - Contains the relative ID of the translated SID.  The
//         remainder of the SID (the prefix) is obtained using the
//         DomainIndex field.
//
//     DomainIndex - Is the index of an entry in a related
//         LSA_REFERENCED_DOMAIN_LIST data structure describing the
//         domain in which the account was found.
//
//         If there is no corresponding reference domain for an entry, then
//         this field will contain a negative value.
//


//
// The following data type is used in SID to name lookup services to
// describe the domains referenced in the lookup operation.
//

 TLsaTranslatedName = packed record
   Use : SID_NAME_USE;
   Name : TLsaUnicodeString;
   DomainIndex : LongInt
 end;
 PLsaTranslatedName = ^TLsaTranslatedName;

// where the members have the following usage:
//
//     Use - Identifies the use of the name.  If this value is SidUnknown
//         or SidInvalid, then the remainder of the record is not set and
//         should be ignored.  If this value is SidWellKnownGroup then the
//         Name field is invalid, but the DomainIndex field is not.
//
//     Name - Contains the isolated name of the translated SID.
//
//     DomainIndex - Is the index of an entry in a related
//         LSA_REFERENCED_DOMAIN_LIST data structure describing the domain
//         in which the account was found.
//
//         If there is no corresponding reference domain for an entry, then
//         this field will contain a negative value.
//


//
// The following data type is used to represent the role of the LSA
// server (primary or backup).
//

 TPolicyLSAServerRole = (sr0, sr1,
   PolicyServerRoleBackup,
   PolicyServerRolePrimary
 );
 PPolicyLSAServerRole = ^TPolicyLSAServerRole;


//
// The following data type is used to represent the state of the LSA
// server (enabled or disabled).  Some operations may only be performed on
// an enabled LSA server.
//

 TPolicyServerEnableState = (se0, se1,
   PolicyServerEnabled,
   PolicyServerDisabled
 );
 PPolicyServerEnableState = ^TPolicyServerEnableState;

//
// The following data type is used to specify the auditing options for
// an Audit Event Type.
//

 POLICY_AUDIT_EVENT_OPTIONS = ULONG;
 PPOLICY_AUDIT_EVENT_OPTIONS = ^POLICY_AUDIT_EVENT_OPTIONS;

// where the following flags can be set:
//
//     POLICY_AUDIT_EVENT_UNCHANGED - Leave existing auditing options
//         unchanged for events of this type.  This flag is only used for
//         set operations.  If this flag is set, then all other flags
//         are ignored.
//
//     POLICY_AUDIT_EVENT_NONE - Cancel all auditing options for events
//         of this type.  If this flag is set, the success/failure flags
//         are ignored.
//
//     POLICY_AUDIT_EVENT_SUCCESS - When auditing is enabled, audit all
//         successful occurrences of events of the given type.
//
//     POLICY_AUDIT_EVENT_FAILURE - When auditing is enabled, audit all
//         unsuccessful occurrences of events of the given type.
//

//
// The following data type is used to return information about privileges
// defined on a system.
//

 TPolicyPrivilegeDefinition = packed record
   name : TLsaUnicodeString;
   LocalValue : TLargeInteger;
 end;
 PPolicyPrivilegeDefinition = ^TPolicyPrivilegeDefinition;

// where the members have the following usage:
//
//     Name - Is the architected name of the privilege.  This is the
//         primary key of the privilege and the only value that is
//         transportable between systems.
//
//     Luid - is a LUID value assigned locally for efficient representation
//         of the privilege.  Ths value is meaningful only on the system it
//         was assigned on and is not transportable in any way.
//


//
// The following data type defines the classes of Policy Information
// that may be queried/set.
//

 POLICY_INFORMATION_CLASS = (pic0,
   PolicyAuditLogInformation,
   PolicyAuditEventsInformation,
   PolicyPrimaryDomainInformation,
   PolicyPdAccountInformation,
   PolicyAccountDomainInformation,
   PolicyLsaServerRoleInformation,
   PolicyReplicaSourceInformation,
   PolicyDefaultQuotaInformation,
   PolicyModificationInformation,
   PolicyAuditFullSetInformation,
   PolicyAuditFullQueryInformation);
 PPOLICY_INFORMATION_CLASS = ^POLICY_INFORMATION_CLASS;


//
// The following data type corresponds to the PolicyAuditLogInformation
// information class.  It is used to represent information relating to
// the Audit Log.
//
// This structure may be used in both query and set operations.  However,
// when used in set operations, some fields are ignored.
//

 TPolicyAuditLogInfo = packed record
   AuditLogPercentFull : ULONG;
   MaximumLogSize : ULONG;
   AuditRetentionPeriod : TLargeInteger;
   AuditLogFullShutdownInProgress : BOOL;
   TimeToShutdown : TLargeInteger;
   NextAuditRecordId : ULONG
 end;
 PPolicyAuditLogInfo = ^TPolicyAuditLogInfo ;


// where the members have the following usage:
//
//     AuditLogPercentFull - Indicates the percentage of the Audit Log
//         currently being used.
//
//     MaximumLogSize - Specifies the maximum size of the Audit Log in
//         kilobytes.
//
//     AuditRetentionPeriod - Indicates the length of time that Audit
//         Records are to be retained.  Audit Records are discardable
//         if their timestamp predates the current time minus the
//         retention period.
//
//     AuditLogFullShutdownInProgress - Indicates whether or not a system
//         shutdown is being initiated due to the security Audit Log becoming
//         full.  This condition will only occur if the system is configured
//         to shutdown when the log becomes full.
//
//         TRUE indicates that a shutdown is in progress
//         FALSE indicates that a shutdown is not in progress.
//
//         Once a shutdown has been initiated, this flag will be set to
//         TRUE.  If an administrator is able to currect the situation
//         before the shutdown becomes irreversible, then this flag will
//         be reset to false.
//
//         This field is ignored for set operations.
//
//     TimeToShutdown - If the AuditLogFullShutdownInProgress flag is set,
//         then this field contains the time left before the shutdown
//         becomes irreversible.
//
//         This field is ignored for set operations.
//


//
// The following data type corresponds to the PolicyAuditEventsInformation
// information class.  It is used to represent information relating to
// the audit requirements.
//

 TPolicyAuditEventsInfo = packed record
   AuditingMode : BOOL;
   EventAuditingOptions : PPOLICY_AUDIT_EVENT_OPTIONS;
   MaximumAuditEventCount : ULONG
 end;
 PPolicyAuditEventsInfo = ^TPolicyAuditEventsInfo;

// where the members have the following usage:
//
//     AuditingMode - A Boolean variable specifying the Auditing Mode value.
//         This value is interpreted as follows:
//
//         TRUE - Auditing is to be enabled (set operations) or is enabled
//             (query operations).  Audit Records will be generated according
//             to the Event Auditing Options in effect (see the
//             EventAuditingOptions field.
//
//         FALSE - Auditing is to be disabled (set operations) or is
//             disabled (query operations).  No Audit Records will be
//             generated.  Note that for set operations the Event Auditing
//             Options in effect will still be updated as specified by the
//             EventAuditingOptions field whether Auditing is enabled or
//             disabled.
//
//    EventAuditingOptions - Pointer to an array of Auditing Options
//        indexed by Audit Event Type.
//
//    MaximumAuditEventCount - Specifiesa count of the number of Audit
//        Event Types specified by the EventAuditingOptions parameter.  If
//        this count is less than the number of Audit Event Types supported
//        by the system, the Auditing Options for Event Types with IDs
//        higher than (MaximumAuditEventCount + 1) are left unchanged.
//


//
// The following structure corresponds to the PolicyAccountDomainInformation
// information class.
//

 TPolicyAccountDomainInfo = packed record
   DomainName : TLsaUnicodeString;
   DomainSid : PSID
 end;
 PPolicyAccountDomainInfo = ^TPolicyAccountDomainInfo;

// where the members have the following usage:
//
//     DomainName - Is the name of the domain
//
//     DomainSid - Is the Sid of the domain
//


//
// The following structure corresponds to the PolicyPrimaryDomainInformation
// information class.
//

 TPolicyPrimaryDomainInfo = packed record
   Name : TLsaUnicodeString;
   Sid : PSID
 end;
 PPolicyPrimaryDomainInfo = ^TPolicyPrimaryDomainInfo;


// where the members have the following usage:
//
//     Name - Is the name of the domain
//
//     Sid - Is the Sid of the domain
//


//
// The following structure corresponds to the PolicyPdAccountInformation
// information class.  This structure may be used in Query operations
// only.
//

 TPolicyPDAccountInfo = packed record
   Name : TLsaUnicodeString
 end;
 PPolicyPDAccountInfo = ^TPolicyPDAccountInfo;

// where the members have the following usage:
//
//     Name - Is the name of an account in the domain that should be used
//         for authentication and name/ID lookup requests.
//


//
// The following structure corresponds to the PolicyLsaServerRoleInformation
// information class.
//

 TPolicyLSAServerRoleInfo = packed record
   LsaServerRole : TPolicyLsaServerRole
 end;
 PPolicyLSAServerRoleInfo = ^TPolicyLSAServerRoleInfo;
 
// where the fields have the following usage:
//
// TBS
//


//
// The following structure corresponds to the PolicyReplicaSourceInformation
// information class.
//

 TPolicyReplicaSourceInfo = packed record
   ReplicaSource : TLsaUnicodeString;
   ReplicaAccountName : TLsaUnicodeString
 end;
 PPolicyReplicaSourceInfo = ^TPolicyReplicaSourceInfo;


//
// The following structure corresponds to the PolicyDefaultQuotaInformation
// information class.
//

 TPolicyDefaultQuotaInfo = packed record
   QuotaLimits : TQuotaLimits
 end;
 PPolicyDefaultQuotaInfo = ^TPolicyDefaultQuotaInfo;


//
// The following structure corresponds to the PolicyModificationInformation
// information class.
//

 TPolicyModificationInfo = packed record
   ModifiedId : TLargeInteger;
   DatabaseCreationTime : TLargeInteger
 end;
 PPolicyModificationInfo = ^TPolicyModificationInfo;


// where the members have the following usage:
//
//     ModifiedId - Is a 64-bit unsigned integer that is incremented each
//         time anything in the LSA database is modified.  This value is
//         only modified on Primary Domain Controllers.
//
//     DatabaseCreationTime - Is the date/time that the LSA Database was
//         created.  On Backup Domain Controllers, this value is replicated
//         from the Primary Domain Controller.
//

//
// The following structure type corresponds to the PolicyAuditFullSetInformation
// Information Class.
//

 TPolicyAuditFullSetInfo = packed record
   ShutDownOnFull : BOOL;
 end;
 PPolicyAuditFullSetInfo = ^TPolicyAuditFullSetInfo;

//
// The following structure type corresponds to the PolicyAuditFullQueryInformation
// Information Class.
//

 TPolicyAuditFullQueryInfo = packed record
   ShutDownOnFull : BOOL;
   LogIsFull : BOOL
 end;
 PPolicyAuditFullQueryInfo = ^TPolicyAuditFullQueryInfo;

//
// LSA RPC Context Handle (Opaque form).  Note that a Context Handle is
// always a pointer type unlike regular handles.
//

 LSA_HANDLE = pointer;
 PLSA_HANDLE = ^LSA_HANDLE;

//
// Trusted Domain Object specific data types
//

//
// This data type defines the following information classes that may be
// queried or set.
//

 TTrustedInformationClass = (tic0,
   TrustedDomainNameInformation,
   TrustedControllersInformation,
   TrustedPosixOffsetInformation,
   TrustedPasswordInformation
 );
 PTrustedInformationClass = ^TTrustedInformationClass;

//
// The following data type corresponds to the TrustedDomainNameInformation
// information class.
//

 TTrustedDomainNameInfo = packed record
   Name : TLSAUnicodeString;
 end;
 PTrustedDomainNameInfo = ^TTrustedDomainNameInfo;

// where members have the following meaning:
//
// Name - The name of the Trusted Domain.
//

//
// The following data type corresponds to the TrustedControllersInformation
// information class.
//

 TTrustedControllersInfo = packed record
   Entries : ULONG;
   Names : PLSAUnicodeString
 end;
 PTrustedControllersInfo = ^TTrustedControllersInfo;

// where members have the following meaning:
//
// Entries - Indicate how mamy entries there are in the Names array.
//
// Names - Pointer to an array of LSA_UNICODE_STRING structures containing the
//     names of domain controllers of the domain.  This information may not
//     be accurate and should be used only as a hint.  The order of this
//     list is considered significant and will be maintained.
//
//     By convention, the first name in this list is assumed to be the
//     Primary Domain Controller of the domain.  If the Primary Domain
//     Controller is not known, the first name should be set to the NULL
//     string.
//


//
// The following data type corresponds to the TrustedPosixOffsetInformation
// information class.
//

 TTrustedPosixOffsetInfo = packed record
   Offset : ULONG
 end;
 PTrustedPosixOffsetInfo = ^TTrustedPosixOffsetInfo;

// where members have the following meaning:
//
// Offset - Is an offset to use for the generation of Posix user and group
//     IDs from SIDs.  The Posix ID corresponding to any particular SID is
//     generated by adding the RID of that SID to the Offset of the SID's
//     corresponding TrustedDomain object.
//

//
// The following data type corresponds to the TrustedPasswordInformation
// information class.
//

 TTrustedPasswordInfo = packed record
   Password : TLsaUnicodeString;
   OldPassword : TLsaUnicodeString
 end;
 PTrustedPasswordInfo = ^TTrustedPasswordInfo;

//
// LSA Enumeration Context
//

 LSA_ENUMERATION_HANDLE = ULONG;
 PLSA_ENUMERATION_HANDLE = ^LSA_ENUMERATION_HANDLE;

//
// LSA Enumeration Information
//

 TLSAEnumerationInformation = packed record
   Sid : PSID
 end;
 PLSAEnumerationInformation = ^TLSAEnumerationInformation;

 function LsaRegisterLogonProcess (
   const LogonProcessName : TLSAString;
   var lsaHandle : THandle;
   var securityMode : LSA_OPERATIONAL_MODE) : NTSTATUS; stdcall;

 function LsaLogonUser (
   LsaHandle : THandle;
   const OriginName : TLsaString;
   LogonType : TSecurityLogonType;
   AuthenticationPackage : ULONG;
   AuthenticationInformation : pointer;
   AuthenticationInformationLength : ULONG;
   LocalGroups : PTokenGroups;
   SourceContext : PTokenSource;
   var ProfileBuffer : pointer;
   var ProfileBufferLength : ULONG;
   var LogonId : TLargeInteger;
   var Token : THandle;
   var Quotas : TQuotaLimits;
   var SubStatus : NTSTATUS) : NTSTATUS; stdcall;

 function LsaLookupAuthenticationPackage (
   LsaHandle : THandle;
   const PackageName : TLSAString;
   var AuthenticationPackage : ULONG) : NTSTATUS; stdcall;

 function LsaFreeReturnBuffer (buffer : pointer) : NTSTATUS; stdcall;

 function LsaCallAuthenticationPackage (
   LsaHandle : THandle;
   AuthenticationPackage : ULONG;
   ProtocolSubmitBuffer : pointer;
   SubmitBufferLength : ULONG;
   var ProtocolReturnBuffer : pointer;
   var ReturnBufferLength : ULONG;
   var ProtocolStatus : NTStatus) : NTSTATUS; stdcall;

 function LsaDeregisterLogonProcess (LSAHandle : THandle) : NTSTATUS; stdcall;
 function LsaConnectUntrusted (LSAHandle : THandle) : NTSTATUS; stdcall;

////////////////////////////////////////////////////////////////////////////
//                                                                        //
// Local Security Policy - Miscellaneous API function prototypes          //
//                                                                        //
////////////////////////////////////////////////////////////////////////////


 function LsaFreeMemory (buffer : pointer) : NTSTATUS; stdcall;
 function LsaClose (ObjectHandle : LSA_HANDLE) : NTSTATUS; stdcall;

 function LsaOpenPolicy(
   const SystemName : TLsaUnicodeString;
   const ObjectAttributes : TLsaObjectAttributes;
   DesiredAccess : ACCESS_MASK;
   var PolicyHandle : LSA_HANDLE) : NTSTATUS; stdcall;

 function LsaQueryInformationPolicy(
   PolicyHandle : LSA_HANDLE;
   InformationClass : Policy_Information_Class;
   var buffer : pointer) : NTSTATUS; stdcall;

 function LsaSetInformationPolicy(
   PolicyHandle : LSA_HANDLE;
   InformationClass : Policy_Information_Class;
   buffer : pointer) : NTSTATUS; stdcall;

 function LsaEnumerateTrustedDomains(
   PolicyHandle : LSA_HANDLE;
   var EnumerationContext : LSA_ENUMERATION_HANDLE;
   var buffer : pointer;
   PreferedMaximumLength : ULONG;
   var CountReturned : ULONG) : NTSTATUS; stdcall;


 function LsaLookupNames(
   PolicyHandle : LSA_HANDLE;
   count : ULONG;
   Names : PLSAUnicodeString;
   var ReferencedDomains : PLSAReferencedDomainList;
   var Sids : PLSATranslatedSID) : NTSTATUS; stdcall;

 function LsaLookupSids(
   PolicyHandle : LSA_HANDLE;
   count : ULONG;
   const sids : PSID;
   var ReferencedDomains : PLSAReferencedDomainList;
   var names : PLSATranslatedName) : NTSTATUS; stdcall;

//
// This new API returns all the accounts with a certain privilege
//

 function LsaEnumerateAccountsWithUserRight(
   PolicyHandle : LSA_HANDLE;
   UserRights : PLsaUnicodeString;
   var EnumerationBuffer : PBYTE;
   var CountReturned : ULONG) : NTSTATUS; stdcall;

//
// These new APIs differ by taking a SID instead of requiring the caller
// to open the account first and passing in an account handle
//

 function LsaEnumerateAccountRights(
   PolicyHandle : LSA_HANDLE;
   AccountSid : PSID;
   var UserRights : PLsaUnicodeString;
   var CountOfRights : ULONG) : NTSTATUS; stdcall;

 function LsaAddAccountRights(
   PolicyHandle : LSA_HANDLE;
   AccountSid : PSID;
   const UserRights : PLsaUnicodeString;
   CountOfRights : ULONG) : NTSTATUS; stdcall;

 function LsaRemoveAccountRights(
   PolicyHandle : LSA_HANDLE;
   AccountSid : PSID;
   AllRights : BOOL;
   const UserRights : PLsaUnicodeString;
   CountOfRights : ULONG) : NTSTATUS; stdcall;


///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// Local Security Policy - Trusted Domain Object API function prototypes     //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

 function LsaQueryTrustedDomainInfo(
   PolicyHandle : LSA_HANDLE;
   TrustedDomainSID : PSID;
   InformationClass : TTrustedInformationClass;
   var buffer : PBYTE) : NTSTATUS; stdcall;

 function LsaSetTrustedDomainInformation(
   PolicyHandle : LSA_HANDLE;
   TrustedDomainSID : PSID;
   InformationClass : TTrustedInformationClass;
   buffer : PBYTE) : NTSTATUS; stdcall;

 function LsaDeleteTrustedDomain(
   PolicyHandle : LSA_HANDLE;
   TrustedDomainSID : PSID) : NTSTATUS; stdcall;

//
// This API sets the workstation password (equivalent of setting/getting
// the SSI_SECRET_NAME secret)
//

 function LsaStorePrivateData (
   PolicyHandle : LSA_HANDLE;
   const KeyName : TLsaUnicodeString;
   const PrivateData : TLsaUnicodeString) : NTSTATUS; stdcall;

 function LsaRetrievePrivateData(
   PolicyHandle : LSA_HANDLE;
   const KeyName : TLsaUnicodeString;
   var PrivateData : TLsaUnicodeString) : NTSTATUS; stdcall;

 function LsaNtStatusToWinError(status : NTStatus) : ULONG; stdcall;

type
 PSamPasswordNotificationRoutine = function (var UserName : TLsaUnicodeString; RelativeID : ULONG; var NewPassword : TLsaUnicodeString) : NTSTATUS; stdcall;

const
 SAM_PASSWORD_CHANGE_NOTIFY_ROUTINE = 'PasswordChangeNotify';
 SAM_INIT_NOTIFICATION_ROUTINE  = 'InitializeChangeNotify';
 SAM_PASSWORD_FILTER_ROUTINE  = 'PasswordFilter';

type
 PSamInitNotificationRoutine = function : BOOL; stdcall;
 PSamPasswordFilterRoutine = function (AccountName, FullName, Password : TLSAUnicodeString; SetOperation : BOOL) : BOOL; stdcall;

 TLsaUnicodeStr = class
   Value : TLsaUnicodeString;
   public
   constructor CreateFromStr (const st : string);
   constructor Create (size : Integer);
   destructor Destroy; override;
 end;

function LSAUnicodeStringToStr (const lsaStr : TLSAUnicodeString) : string;

implementation

constructor TLsaUnicodeStr.CreateFromStr (const st : string);
var
 len : Integer;
 wst : WideString;
begin
 len := Length (st);
 Value.Length := len * sizeof (WideChar);
 Value.MaximumLength := (len + 1) * sizeof (WideChar);
 GetMem (Value.buffer, sizeof (WideChar) * (len + 1));
 wst := st;
 lstrcpyw (Value.buffer, PWideChar (wst))
end;

constructor TLsaUnicodeStr.Create (size : Integer);
begin
 Value.MaximumLength := size;
 Value.Length := 0;
 GetMem (Value.Buffer, sizeof (WideChar) * (size + 1))
end;

destructor TLsaUnicodeStr.Destroy;
begin
 ReallocMem (Value.Buffer, 0);
 inherited
end;

function LSAUnicodeStringToStr (const lsaStr : TLSAUnicodeString) : string;
{$IFDEF UNICODE}
begin
  Result:=string(lsaStr.Buffer);
end;
{$ELSE}
var
 len : Integer;
begin
 len := lsaStr.Length div sizeof (WideChar);
 SetLength (result, len);
 WideCharToMultiByte (CP_ACP, 0, lsaStr.Buffer, len,  PAnsiChar (result), len, Nil, Nil);
end;
{$ENDIF}

const LSADll = 'advapi32.dll';

function LsaRegisterLogonProcess;           external LSADll;
function LsaLogonUser;                      external LSADll;
function LsaLookupAuthenticationPackage;    external LSADll;
function LsaFreeReturnBuffer;               external LSADll;
function LsaCallAuthenticationPackage;      external LSADll;
function LsaDeregisterLogonProcess;         external LSADll;
function LsaConnectUntrusted;               external LSADll;
function LsaFreeMemory;                     external LSADll;
function LsaClose;                          external LSADll;
function LsaOpenPolicy;                     external LSADll;
function LsaQueryInformationPolicy;         external LSADll;
function LsaSetInformationPolicy;           external LSADll;
function LsaEnumerateTrustedDomains;        external LSADll;
function LsaLookupNames;                    external LSADll;
function LsaLookupSids;                     external LSADll;
function LsaEnumerateAccountsWithUserRight; external LSADll;
function LsaEnumerateAccountRights;         external LSADll;
function LsaAddAccountRights;               external LSADll;
function LsaRemoveAccountRights;            external LSADll;
function LsaQueryTrustedDomainInfo;         external LSADll;
function LsaSetTrustedDomainInformation;    external LSADll;
function LsaDeleteTrustedDomain;            external LSADll;
function LsaStorePrivateData;               external LSADll;
function LsaRetrievePrivateData;            external LSADll;
function LsaNtStatusToWinError;             external LSADll;

end.
