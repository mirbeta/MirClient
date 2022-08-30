{*******************************************************}
{                 MiTeC Common Routines                 }
{                     Windows IOCTL                     }
{                                                       }
{                                                       }
{         Copyright (c) by 1997-2019 Michal Mutl        }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_WinIOCTL;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils;
     {$ELSE}
     Windows, SysUtils;
     {$ENDIF}

const
  GUID_DEVINTERFACE_DISK: TGUID = (
    D1:$53f56307; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_CDROM: TGUID = (
    D1:$53f56308; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_PARTITION: TGUID = (
    D1:$53f5630a; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_TAPE: TGUID = (
    D1:$53f5630b; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_WRITEONCEDISK: TGUID = (
    D1:$53f5630c; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_VOLUME: TGUID = (
    D1:$53f5630d; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_MEDIUMCHANGER: TGUID = (
    D1:$53f56310; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_FLOPPY: TGUID = (
    D1:$53f56311; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_CDCHANGER: TGUID = (
    D1:$53f56312; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_STORAGEPORT: TGUID = (
    D1:$2accfe60; D2:$c130; D3:$11d2; D4:($b0, $82, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_COMPORT: TGUID = (
    D1:$86e0d1e0; D2:$8089; D3:$11d0; D4:($9c, $e4, $08, $00, $3e, $30, $1f, $73));
  GUID_DEVINTERFACE_SERENUM_BUS_ENUMERATOR: TGUID = (
    D1:$4D36E978; D2:$E325; D3:$11CE; D4:($BF, $C1, $08, $00, $2B, $E1, $03, $18));
  GUID_DEVINTERFACE_USB_DEVICE: TGUID = '{A5DCBF10-6530-11D2-901F-00C04FB951ED}';  

type
  TSrbIoControl = packed record
    HeaderLength : Cardinal;
    Signature    : Array[0..7] of AnsiChar;
    Timeout      : Cardinal;
    ControlCode  : Cardinal;
    ReturnCode   : Cardinal;
    Length       : Cardinal;
  end;
  SRB_IO_CONTROL = TSrbIoControl;
  PSrbIoControl = ^TSrbIoControl;

  TIDERegs = packed record
    bFeaturesReg     : Byte; // Used for specifying SMART "commands".
    bSectorCountReg  : Byte; // IDE sector count register
    bSectorNumberReg : Byte; // IDE sector number register
    bCylLowReg       : Byte; // IDE low order cylinder value
    bCylHighReg      : Byte; // IDE high order cylinder value
    bDriveHeadReg    : Byte; // IDE drive/head register
    bCommandReg      : Byte; // Actual IDE command.
    bReserved        : Byte; // reserved.  Must be zero.
  end;
  IDEREGS   = TIDERegs;
  PIDERegs  = ^TIDERegs;

  TSendCmdInParams = packed record
    cBufferSize  : Cardinal;
    irDriveRegs  : TIDERegs;
    bDriveNumber : Byte;
    bReserved    : Array[0..2] of Byte;
    dwReserved   : Array[0..3] of Cardinal;
    bBuffer      : Array[0..0] of Byte;
  end;
  SENDCMDINPARAMS   = TSendCmdInParams;
  PSendCmdInParams  = ^TSendCmdInParams;

   // Status returned from driver
  TDriverStatus = record
    bDriverError,  //  Error code from driver, or 0 if no error.
    bIDEStatus: BYTE;    //  Contents of IDE Error register.
                        //  Only valid when bDriverError is SMART_IDE_ERROR.
    bReserved: array[0..1] of BYTE;  //  Reserved for future expansion.
    dwReserved: array[0..1] of Cardinal;  //  Reserved for future expansion.
  end;
  DRIVERSTATUS = TDriverStatus;
  PDriverStatus = ^TDriverStatus;

     // Structure returned by PhysicalDrive IOCTL for several commands
  TSendCmdOutParams = record
    cBufferSize: Cardinal;   //  Size of bBuffer in bytes
    DriverStatus: TDriverStatus;  //  Driver status structure.
    bBuffer: array [0..511] of ansichar;    //  Buffer of arbitrary length in which to store the data read from the                                                       // drive.
  end;
  SENDCMDOUTPARAMS = TSendCmdOutParams;
  PSendCmdOutParams = ^TSendCmdOutParams;

	_IDENTIFY_DEVICE_DATA = packed record
		GeneralConfiguration: Word;
      { =
      USHORT Reserved1  :1;
      USHORT Retired3  :1;
      USHORT ResponseIncomplete  :1;
      USHORT Retired2  :3;
      USHORT FixedDevice  :1;
      USHORT RemovableMedia  :1;
      USHORT Retired1  :7;
      USHORT DeviceType  :1;}
		NumCylinders: Word;
		ReservedWord2: Word;
		NumHeads: Word;
		Retired1: array[0..1] of Word;
		NumSectorsPerTrack: Word;
		VendorUnique1: array[0..2] of Word;
		SerialNumber: array[0..19] of AnsiChar;
		Retired2: array[0..1] of Word;
		Obsolete1: Word;
		FirmwareRevision: array[0..7] of AnsiChar;
		ModelNumber: array[0..39] of AnsiChar;
		MaximumBlockTransfer: Byte;
		VendorUnique2: Byte;
		ReservedWord48: Word;
    ReservedByte49: Byte;
		Capabilities: Byte;
      { =
      UCHAR  DmaSupported  :1;
      UCHAR  LbaSupported  :1;
      UCHAR  IordyDisable  :1;
      UCHAR  IordySupported  :1;
      UCHAR  Reserved1  :1;
      UCHAR  StandybyTimerSupport  :1;
      UCHAR  Reserved2  :2;}
    ReservedWord50: Word;
		ObsoleteWords51: array[0..1] of Word;
		TranslationFieldsValid: Word;
      { =
      USHORT TranslationFieldsValid  :3;
      USHORT Reserved3  :13;}
		NumberOfCurrentCylinders: Word;
		NumberOfCurrentHeads: Word;
		CurrentSectorsPerTrack: Word;
		CurrentSectorCapacity: Cardinal;
		CurrentMultiSectorSetting: Byte;
		MultiSectorSettingValid: Byte;
      { =
      UCHAR  MultiSectorSettingValid  :1;
      UCHAR  ReservedByte59  :7;}
		UserAddressableSectors: Cardinal;
		ObsoleteWord62: Word;
    MultiWordDMAActive: Byte;
		MultiWordDMASupport: Byte;
    ReservedByte64: Byte;
    AdvancedPIOModes: Byte;
		MinimumMWXferCycleTime: Word;
		RecommendedMWXferCycleTime: Word;
		MinimumPIOCycleTime: Word;
		MinimumPIOCycleTimeIORDY: Word;
		ReservedWords69: array[0..5] of Word;
		QueueDepth: Word;
      { =
      USHORT QueueDepth  :5;
      USHORT ReservedWord75  :11;}
		ReservedWords76: array[0..3] of Word;
		MajorRevision: Word;
		MinorRevision: Word;
		CommandSetSupport: array[0..5] of byte;
      { =
      USHORT SmartCommands  :1;
      USHORT SecurityMode  :1;
      USHORT RemovableMediaFeature  :1;
      USHORT PowerManagement  :1;
      USHORT Reserved1  :1;
      USHORT WriteCache  :1;
      USHORT LookAhead  :1;
      USHORT ReleaseInterrupt  :1;
      USHORT ServiceInterrupt  :1;
      USHORT DeviceReset  :1;
      USHORT HostProtectedArea  :1;
      USHORT Obsolete1  :1;
      USHORT WriteBuffer  :1;
      USHORT ReadBuffer  :1;
      USHORT Nop  :1;
      USHORT Obsolete2  :1;
      USHORT DownloadMicrocode  :1;
      USHORT DmaQueued  :1;
      USHORT Cfa  :1;
      USHORT AdvancedPm  :1;
      USHORT Msn  :1;
      USHORT PowerUpInStandby  :1;
      USHORT ManualPowerUp  :1;
      USHORT Reserved2  :1;
      USHORT SetMax  :1;
      USHORT Acoustics  :1;
      USHORT BigLba  :1;
      USHORT DeviceConfigOverlay  :1;
      USHORT FlushCache  :1;
      USHORT FlushCacheExt  :1;
      USHORT Resrved3  :2;
      USHORT SmartErrorLog  :1;
      USHORT SmartSelfTest  :1;
      USHORT MediaSerialNumber  :1;
      USHORT MediaCardPassThrough  :1;
      USHORT StreamingFeature  :1;
      USHORT GpLogging  :1;
      USHORT WriteFua  :1;
      USHORT WriteQueuedFua  :1;
      USHORT WWN64Bit  :1;
      USHORT URGReadStream  :1;
      USHORT URGWriteStream  :1;
      USHORT ReservedForTechReport  :2;
      USHORT IdleWithUnloadFeature  :1;
      USHORT Reserved4  :2;}
		CommandSetActive: array[0..5] of byte;
      { = same as CommandSetSupport}
    UltraDMAActive: Byte;
    UltraDMASupport: Byte;
		ReservedWord89: array[0..3] of Word;
		HardwareResetResult: Word;
    RecommendedAcousticValue: Byte;
		CurrentAcousticValue: Byte;
		ReservedWord95: array[0..4] of Word;
		Max48BitLBA: array[0..1] of Cardinal;
		StreamingTransferTime: Word;
		ReservedWord105: Word;
		PhysicalLogicalSectorSize: Word;
      { =
      USHORT LogicalSectorsPerPhysicalSector  :4;
      USHORT Reserved0  :8;
      USHORT LogicalSectorLongerThan256Words  :1;
      USHORT MultipleLogicalSectorsPerPhysicalSector  :1;
      USHORT Reserved1  :2;}
		InterSeekDelay: Word;
		WorldWideName: array[0..7] of Byte;
		ReservedForWorldWideName128: array[0..7] of Byte;
		ReservedForTlcTechnicalReport: Word;
		WordsPerLogicalSector: array[0..1] of Word;
		CommandSetSupportExt: Word;
      { =
      USHORT ReservedForDrqTechnicalReport  :1;
      USHORT WriteReadVerifySupported  :1;
      USHORT Reserved01  :11;
      USHORT Reserved1  :2;}
		CommandSetActiveExt: Word;
      { = same as CommandSetSupportExt}
		ReservedForExpandedSupportandActive: array[0..5] of Word;
		MsnSupport: Word;
      { =
      USHORT MsnSupport  :2;
      USHORT ReservedWord1274  :14;}
		SecurityStatus: Word;
      { =
      USHORT SecuritySupported  :1;
      USHORT SecurityEnabled  :1;
      USHORT SecurityLocked  :1;
      USHORT SecurityFrozen  :1;
      USHORT SecurityCountExpired  :1;
      USHORT EnhancedSecurityEraseSupported  :1;
      USHORT Reserved0  :2;
      USHORT SecurityLevel  :1;
      USHORT Reserved1  :7;}
		ReservedWord129: array[0..30] of Word;
		CfaPowerModel: Word;
      { =
      USHORT MaximumCurrentInMA2  :12;
      USHORT CfaPowerMode1Disabled  :1;
      USHORT CfaPowerMode1Required  :1;
      USHORT Reserved0  :1;
      USHORT Word160Supported  :1;}
		ReservedForCfaWord161: array[0..7] of Word;
		DataSetManagementFeature: Word;
      { =
      USHORT SupportsTrim  :1;
      USHORT Reserved0  :15;}
		ReservedForCfaWord170: array[0..5] of Word;
		CurrentMediaSerialNumber: array[0..29] of Word;
		ReservedWord206: Word;
		ReservedWord207: array[0..1] of Word;
		BlockAlignment: Word;
      { =
      USHORT AlignmentOfLogicalWithinPhysical  :14;
      USHORT Word209Supported  :1;
      USHORT Reserved0  :1;}
		WriteReadVerifySectorCountMode3Only: array[0..1] of Word;
		WriteReadVerifySectorCountMode2Only: array[0..1] of Word;
		NVCacheCapabilities: Word;
      { =
      USHORT NVCachePowerModeEnabled  :1;
      USHORT Reserved0  :3;
      USHORT NVCacheFeatureSetEnabled  :1;
      USHORT Reserved1  :3;
      USHORT NVCachePowerModeVersion  :4;
      USHORT NVCacheFeatureSetVersion  :4;}
		NVCacheSizeLSW: Word;
		NVCacheSizeMSW: Word;
		NominalMediaRotationRate: Word;
		ReservedWord218: Word;
		NVCacheEstimatedTimeToSpinUpInSeconds: Byte;
    Reserved: Byte;
		ReservedWord220: array[0..34] of Word;
    CheckSum: Byte;
		Signature: Byte;
	end;
	IDENTIFY_DEVICE_DATA = _IDENTIFY_DEVICE_DATA;
	{$EXTERNALSYM IDENTIFY_DEVICE_DATA}
	PIDENTIFY_DEVICE_DATA = ^_IDENTIFY_DEVICE_DATA;
	{$EXTERNALSYM PIDENTIFY_DEVICE_DATA}
  TIdentifyDeviceData = IDENTIFY_DEVICE_DATA;
  PIdentifyDeviceData = ^TIdentifyDeviceData;

  TDriveAttribute = packed record
     bAttrID: byte;		// Identifies which attribute
     wStatusFlags: word;	// see bit definitions below
     bAttrValue: byte;		// Current normalized value
     bWorstValue: byte;	// How bad has it ever been?
     bRawValue: array[0..5] of byte;	// Un-normalized value
     bReserved: byte;		// ...
  end;
  PDriveAttribute = ^TDriveAttribute;

  TAttrThreshold = packed record
    bAttrID: Byte;
    bWarrantyThreshold: Byte;
    bReserved: array[0..9] of Byte;
  end;
  PAttrThreshold = ^TAttrThreshold;

  TGetVersionOutParams = record
    bVersion,      // Binary driver version.
    bRevision,     // Binary driver revision.
    bReserved,     // Not used.
    bIDEDeviceMap: BYTE; // Bit map of IDE devices.
    fCapabilities: Cardinal; // Bit mask of driver capabilities.
    dwReserved: array[0..3] of Cardinal; // For future use.
  end;
  GETVERSIONOUTPARAMS = TGetVersionOutParams;
  PGetVersionOutParams = ^TGetVersionOutParams;

  TPartitionInformation = record
    StartingOffset: LARGE_INTEGER;
    Length: LARGE_INTEGER;
    HiddenSectors: Cardinal;
    Number: Cardinal;
    Typ: BYTE;
    BootIndicator: ByteBool;
    Recognized: ByteBool;
    Rewrite: ByteBool;
  end;
  PARTITION_INFORMATION = TPartitionInformation;
  PPartitionInformation = ^TPartitionInformation;

  TDriveLayoutInformation = record
    PartitionCount: Cardinal;
    Signature: Cardinal;
    PartitionEntry: array [0..0] of TPartitionInformation;
  end;
  DRIVE_LAYOUT_INFORMATION = TDriveLayoutInformation;
  PDriveLayoutInformation = ^TDriveLayoutInformation;

  TDiskGeometry = record
    Cylinders: LARGE_INTEGER;
    MediaType: Cardinal;
    TracksPerCylinder: Cardinal;
    SectorsPerTrack: Cardinal;
    BytesPerSector: Cardinal;
  end;
  DISK_GEOMETRY = TDiskGeometry;
  PDiskGeometry = ^TDiskGeometry;

  TDiskExtent = record
    DiskNumber: Cardinal;
    StartingOffset: LARGE_INTEGER;
    ExtentLength: LARGE_INTEGER;
  end;
  DISK_EXTENT = TDiskExtent;
  PDiskExtent = ^TDiskExtent;

  TVolumeDiskExtents = record
    NumberOfDiskExtents: Cardinal;
    Extents: array [0..0] of TDiskExtent;
  end;
  VOLUME_DISK_EXTENTS = TVolumeDiskExtents;
  PVolumeDiskExtents = ^TVolumeDiskExtents;

  TDiskControllerNumber = record
    ControllerNumber: Cardinal;
    DiskNumber: Cardinal;
  end;
  DISK_CONTROLLER_NUMBER = TDiskControllerNumber;
  PDiskControllerNumber = ^TDiskControllerNumber;

  PGET_LENGTH_INFORMATION = ^GET_LENGTH_INFORMATION;
  _GET_LENGTH_INFORMATION = record
    Length: LARGE_INTEGER;
  end;
  GET_LENGTH_INFORMATION = _GET_LENGTH_INFORMATION;
  TGetLengthInformation = GET_LENGTH_INFORMATION;
  PGetLengthInformation = PGET_LENGTH_INFORMATION;

  TScsiPassThrough = record
    Length             : Word;
    ScsiStatus         : Byte;
    PathId             : Byte;
    TargetId           : Byte;
    Lun                : Byte;
    CdbLength          : Byte;
    SenseInfoLength    : Byte;
    DataIn             : Byte;
    DataTransferLength : Cardinal;
    TimeOutValue       : Cardinal;
    DataBufferOffset   : {$IFDEF RAD9PLUS}ULONG_PTR{$ELSE}Cardinal{$ENDIF};
    SenseInfoOffset    : Cardinal;
    Cdb                : Array[0..15] of Byte;
  end;

  TScsiPassThroughDirect = record
    Length             : Word;
    ScsiStatus         : Byte;
    PathId             : Byte;
    TargetId           : Byte;
    Lun                : Byte;
    CdbLength          : Byte;
    SenseInfoLength    : Byte;
    DataIn             : Byte;
    DataTransferLength : Cardinal;
    TimeOutValue       : Cardinal;
    DataBufferOffset   : Pointer;
    SenseInfoOffset    : Cardinal;
    Cdb                : Array[0..15] of Byte;
  end;

  TScsiPassThroughWithBuffers = record
    spt : TScsiPassThrough;
    bSenseBuf : Array[0..31] of Byte;
    bDataBuf : Array[0..191] of Byte;
  end;

  TScsiPassThroughDirectWithBuffers = record
    sptd : TScsiPassThroughDirect;
    dummy: Cardinal;
    bSenseBuf : array[0..31] of Byte;
  end;

  TSCSIAddress = record
    Length : LongInt;
    PortNumber : Byte;
    PathId : Byte;
    TargetId : Byte;
    Lun : Byte;
  end;
  PSCSIAddress = ^TSCSIAddress;

  _ATA_PASS_THROUGH_EX = record
    Length: Word;
    AtaFlags: Word;
    PathId: Byte;
    TargetId: Byte;
    Lun: Byte;
    ReservedAsUchar: Byte;
    DataTransferLength: Cardinal;
    TimeOutValue: Cardinal;
    ReservedAsUlong: Cardinal;
    DataBufferOffset: {$IFDEF RAD9PLUS}ULONG_PTR{$ELSE}Cardinal{$ENDIF};
    PreviousTaskFile: array[0..7] of Byte;
    CurrentTaskFile: array[0..7] of Byte;
  end;
  ATA_PASS_THROUGH_EX = _ATA_PASS_THROUGH_EX;
  TATAPassThroughEx = _ATA_PASS_THROUGH_EX;
  P_ATA_PASS_THROUGH_EX = ^_ATA_PASS_THROUGH_EX;
  PATAPassThroughEx = ^TATAPassThroughEx;

  TATAIdentifyDeviceQuery = record
    Header: ATA_PASS_THROUGH_EX;
    Data: array[0..255] of Word;
  end;
  PATAIdentifyDeviceQuery = ^TATAIdentifyDeviceQuery;

  _ATA_PASS_THROUGH_DIRECT = record
    Length: Word;
    AtaFlags: Word;
    PathId: Byte;
    TargetId: Byte;
    Lun: Byte;
    ReservedAsUchar: Byte;
    DataTransferLength: Cardinal;
    TimeOutValue: Cardinal;
    ReservedAsUlong: Cardinal;
    DataBufferOffset: Pointer;
    PreviousTaskFile: array[0..7] of Byte;
    CurrentTaskFile: array[0..7] of Byte;
  end;
  ATA_PASS_THROUGH_DIRECT = _ATA_PASS_THROUGH_DIRECT;
  TATAPassThroughDirect = _ATA_PASS_THROUGH_DIRECT;
  P_ATA_PASS_THROUGH_DIRECT = ^_ATA_PASS_THROUGH_DIRECT;
  PATAPassThroughDirect = ^TATAPassThroughDirect;

  TATADirectIdentifyDeviceQuery = record
    Header: ATA_PASS_THROUGH_DIRECT;
    Data: array[0..255] of Word;
  end;
  PATADirectIdentifyDeviceQuery = ^TATADirectIdentifyDeviceQuery;

  TCDB6GENERIC = record
    OperationCode: Byte;
    Immediate: Byte;
    CommandUniqueBits: Byte;
    LogicalUnitNumber: Byte;
    CommandUniqueBytes: array[0..2] of Byte;
    Link: Byte;
    Flag: Byte;
    Reserved: Byte;
    VendorUnique: Byte;
  end;

  GDT_DESCRIPTOR = packed record
    Limit_0_15,
    Base_0_15 :  word;
    Base_16_23 : byte;
    bAR : byte;
    {
    BYTE Type         : 4;
    BYTE System       : 1;
    BYTE DPL          : 2;
    BYTE Present      : 1;
    }
    Limit_16_19 : byte;
    {
    BYTE Limit_16_19  : 4;
    BYTE Available    : 1;
    BYTE Reserved     : 1;
    BYTE D_B          : 1;
    BYTE Granularity  : 1;
    }
    Base_24_31 : byte;
  end;

  PGDT_DESCRIPTOR = ^GDT_DESCRIPTOR;

  CALLGATE_DESCRIPTOR = packed record
    Offset_0_15,
    Selector : word;
    bAR : word;
    {
    WORD ParamCount   : 5;
    WORD Unused       : 3;
    WORD Type         : 4;
    WORD System       : 1;
    WORD DPL          : 2;
    WORD Present      : 1;
    }
    Offset_16_31 : word;
  end;

  PCALLGATE_DESCRIPTOR = ^CALLGATE_DESCRIPTOR;

  GDTR = packed record
    wGDTLimit : word;
    dwGDTBase : Cardinal;
  end;

{  TPort32Struct = record
    wPortAddr: Word;
    dwPortVal: Cardinal;
    bSize: Byte;
  end;
  PPort32Struct = ^TPort32Struct;
}

  TDMIDiskInfo = record
    Cylinders: LARGE_INTEGER;
    MediaType: DWORD;
    TracksPerCylinder: DWORD;
    SectorsPerTrack: DWORD;
    BytesPerSector: DWORD;
    NumberMediaSides: DWORD;
    MediaCharacteristics: DWORD; // Bitmask of MEDIA_XXX values.
  end;

  TDMIRemovableDiskInfo = record
    Cylinders: LARGE_INTEGER;
    MediaType: DWORD;
    TracksPerCylinder: DWORD;
    SectorsPerTrack: DWORD;
    BytesPerSector: DWORD;
    NumberMediaSides: DWORD;
    MediaCharacteristics: DWORD; // Bitmask of MEDIA_XXX values.
  end;

  TDMITapeInfo = record
    MediaType: DWORD;
    MediaCharacteristics: DWORD; // Bitmask of MEDIA_XXX values.
    CurrentBlockSize: DWORD;
    BusType: DWORD;
    //
    // Bus specific information describing the medium supported.
    //
    case Integer of {BusSpecificData}
      0: ( {ScsiInformation}
        MediumType: BYTE;
        DensityCode: BYTE);
  end;

  PDEVICE_MEDIA_INFO = ^DEVICE_MEDIA_INFO;
  _DEVICE_MEDIA_INFO = record
    case Integer of
      0: (DiskInfo: TDMIDiskInfo);
      1: (RemovableDiskInfo: TDMIRemovableDiskInfo);
      2: (TapeInfo: TDMITapeInfo);
  end;

  DEVICE_MEDIA_INFO = _DEVICE_MEDIA_INFO;
  TDeviceMediaInfo = DEVICE_MEDIA_INFO;
  PDeviceMediaInfo = PDEVICE_MEDIA_INFO;

  PGET_MEDIA_TYPES = ^GET_MEDIA_TYPES;
  _GET_MEDIA_TYPES = record
    DeviceType: DWORD; // FILE_DEVICE_XXX values
    MediaInfoCount: DWORD;
    MediaInfo: array [0..0] of DEVICE_MEDIA_INFO;
  end;
  GET_MEDIA_TYPES = _GET_MEDIA_TYPES;
  TGetMediaTypes = GET_MEDIA_TYPES;
  PGetMediaTypes = PGET_MEDIA_TYPES;

  PMEDIA_SERIAL_NUMBER_DATA = ^_MEDIA_SERIAL_NUMBER_DATA;
  _MEDIA_SERIAL_NUMBER_DATA = record
    SerialNumberLength,
    Result: Cardinal;
    Reserved: array[0..1] of Cardinal;
    SerialNumberData: array[0..31] of ansichar;
  end;
  MEDIA_SERIAL_NUMBER_DATA = _MEDIA_SERIAL_NUMBER_DATA;

  _STORAGE_PROPERTY_ID = (
    StorageDeviceProperty                 =  0,
    StorageAdapterProperty                =  1,
    StorageDeviceIdProperty               =  2,
    StorageDeviceUniqueIdProperty         =  3,
    StorageDeviceWriteCacheProperty       =  4,
    StorageMiniportProperty               =  5,
    StorageAccessAlignmentProperty        =  6,
    StorageDeviceSeekPenaltyProperty      =  7,
    StorageDeviceTrimProperty             =  8,
    StorageDeviceWriteAggregationProperty =  9,
    StorageDeviceDeviceTelemetryProperty  = 10
  );
  {$EXTERNALSYM _STORAGE_PROPERTY_ID}
  STORAGE_PROPERTY_ID = _STORAGE_PROPERTY_ID;
  {$EXTERNALSYM  STORAGE_PROPERTY_ID}
  TStoragePropertyId  = _STORAGE_PROPERTY_ID;
  PStoragePropertyId  = ^TStoragePropertyId;

  _STORAGE_QUERY_TYPE = (
    PropertyStandardQuery   = 0,
    PropertyExistsQuery     = 1,
    PropertyMaskQuery       = 2,
    PropertyQueryMaxDefined = 3
  );
  {$EXTERNALSYM _STORAGE_QUERY_TYPE}
  STORAGE_QUERY_TYPE = _STORAGE_QUERY_TYPE;
  {$EXTERNALSYM  STORAGE_QUERY_TYPE}
  TStorageQueryType  = _STORAGE_QUERY_TYPE;
  PStorageQueryType  = ^TStorageQueryType;

  _STORAGE_PROPERTY_QUERY = packed record
    PropertyId: DWORD;
    QueryType: DWORD;
    AdditionalParameters: array[0..9] of Byte;
  end;
  {$EXTERNALSYM _STORAGE_PROPERTY_QUERY}
  STORAGE_PROPERTY_QUERY = _STORAGE_PROPERTY_QUERY;
  {$EXTERNALSYM  STORAGE_PROPERTY_QUERY}
  TStoragePropertyQuery  = _STORAGE_PROPERTY_QUERY;
  PStoragePropertyQuery  = ^TStoragePropertyQuery;

  _DEVICE_SEEK_PENALTY_DESCRIPTOR = packed record
    Version: DWORD;
    Size: DWORD;
    IncursSeekPenalty: ByteBool;
    Reserved: array[0..2] of Byte;
  end;
  {$EXTERNALSYM _DEVICE_SEEK_PENALTY_DESCRIPTOR}
  DEVICE_SEEK_PENALTY_DESCRIPTOR = _DEVICE_SEEK_PENALTY_DESCRIPTOR;
  {$EXTERNALSYM  DEVICE_SEEK_PENALTY_DESCRIPTOR}
  TDeviceSeekPenaltyDescriptor   = _DEVICE_SEEK_PENALTY_DESCRIPTOR;
  PDeviceSeekPenaltyDescriptor   = ^TDeviceSeekPenaltyDescriptor;

  _STORAGE_DEVICE_DESCRIPTOR = record
    Version: cardinal; // Sizeof(STORAGE_DEVICE_DESCRIPTOR)
    Size: cardinal; // Total size of the descriptor, including the space for additional data and id strings
    DeviceType: byte; // The SCSI-2 device type
    DeviceTypeModifier: byte; // The SCSI-2 device type modifier (if any) - this may be zero
    RemovableMedia: boolean; // Flag indicating whether the device's media (if any) is removable.  This field should be ignored for media-less devices;
    CommandQueueing: boolean; // Flag indicating whether the device can support mulitple outstanding
                             // commands.  The actual synchronization in this case is the responsibility
                             // of the port driver.
    VendorIdOffset: cardinal; // Byte offset to the zero-terminated ascii string containing the device's
                               // vendor id string.  For devices with no such ID this will be zero
    ProductIdOffset: cardinal; // Byte offset to the zero-terminated ascii string containing the device's
                               // product id string.  For devices with no such ID this will be zero
    ProductRevisionOffset: cardinal; // Byte offset to the zero-terminated ascii string containing the device's
                                     // product revision string.  For devices with no such string this will be zero
    SerialNumberOffset: cardinal; // Byte offset to the zero-terminated ascii string containing the device's
                                 // serial number.  For devices with no serial number this will be zero
    BusType: Cardinal; // Contains the bus type (as defined above) of the device.  It should be
                             // used to interpret the raw device properties at the end of this structure (if any)
    RawPropertiesLength: Cardinal; // The number of bytes of bus-specific data which have been appended to this descriptor;
    RawDeviceProperties: array[0..0] of byte; // Place holder for the first byte of the bus specific property data
  end;
  STORAGE_DEVICE_DESCRIPTOR = _STORAGE_DEVICE_DESCRIPTOR;
  PSTORAGE_DEVICE_DESCRIPTOR = ^_STORAGE_DEVICE_DESCRIPTOR;

  PSTORAGE_DEVICE_NUMBER = ^STORAGE_DEVICE_NUMBER;
  _STORAGE_DEVICE_NUMBER = record
    //
    // The FILE_DEVICE_XXX type for this device.
    //
    DeviceType: DWORD;
    //
    // The number of this device
    //
    DeviceNumber: DWORD;
    //
    // If the device is partitionable, the partition number of the device.
    // Otherwise -1
    //
    PartitionNumber: DWORD;
  end;
  STORAGE_DEVICE_NUMBER = _STORAGE_DEVICE_NUMBER;
  TStorageDeviceNumber = STORAGE_DEVICE_NUMBER;
  PStorageDeviceNumber = PSTORAGE_DEVICE_NUMBER;

  BATTERY_QUERY_INFORMATION_LEVEL = (
    BatteryInformation,
    BatteryGranularityInformation,
    BatteryTemperature,
    BatteryEstimatedTime,
    BatteryDeviceName,
    BatteryManufactureDate,
    BatteryManufactureName,
    BatteryUniqueID,
    BatterySerialNumber);
  TBatteryQueryInformationLevel = BATTERY_QUERY_INFORMATION_LEVEL;

  _BATTERY_QUERY_INFORMATION = record
    BatteryTag: Cardinal;
    InformationLevel: BATTERY_QUERY_INFORMATION_LEVEL;
    AtRate: integer;
  end;
  BATTERY_QUERY_INFORMATION = _BATTERY_QUERY_INFORMATION;
  PBATTERY_QUERY_INFORMATION = ^BATTERY_QUERY_INFORMATION;
  TBatteryQueryInformation = BATTERY_QUERY_INFORMATION;
  PBatteryQueryInformation = PBATTERY_QUERY_INFORMATION;

// Format of data returned when
// BATTERY_INFORMATION_LEVEL = BatteryInformation

  _BATTERY_INFORMATION = record
    Capabilities: Cardinal;
    Technology: Byte;
    Reserved: array [0..2] of Byte;
    Chemistry: array [0..3] of Byte;
    DesignedCapacity: Cardinal;
    FullChargedCapacity: Cardinal;
    DefaultAlert1: Cardinal;
    DefaultAlert2: Cardinal;
    CriticalBias: Cardinal;
    CycleCount: Cardinal;
  end;
  BATTERY_INFORMATION = _BATTERY_INFORMATION;
  PBATTERY_INFORMATION = ^BATTERY_INFORMATION;
  TBatteryInformation = BATTERY_INFORMATION;
  PBatteryInformation = PBATTERY_INFORMATION;

  _BATTERY_WAIT_STATUS = record
    BatteryTag: Cardinal;
    Timeout: Cardinal;
    PowerState: Cardinal;
    LowCapacity: Cardinal;
    HighCapacity: Cardinal;
  end;
  BATTERY_WAIT_STATUS = _BATTERY_WAIT_STATUS;
  PBATTERY_WAIT_STATUS = ^BATTERY_WAIT_STATUS;
  TBatteryWaitStatus = BATTERY_WAIT_STATUS;
  PBatteryWaitStatus = PBATTERY_WAIT_STATUS;

// Structure of output buffer from IOCTL_BATTERY_QUERY_STATUS

  _BATTERY_STATUS = record
    PowerState: Cardinal;
    Capacity: Cardinal;
    Voltage: Cardinal;
    Rate: Longint;
  end;
  BATTERY_STATUS = _BATTERY_STATUS;
  PBATTERY_STATUS = ^BATTERY_STATUS;
  TBatteryStatus = BATTERY_STATUS;
  PBatteryStatus = PBATTERY_STATUS;

  PDISK_PERFORMANCE = ^DISK_PERFORMANCE;
  _DISK_PERFORMANCE = record
    BytesRead: LARGE_INTEGER;
    BytesWritten: LARGE_INTEGER;
    ReadTime: LARGE_INTEGER;
    WriteTime: LARGE_INTEGER;
    IdleTime: LARGE_INTEGER;
    ReadCount: DWORD;
    WriteCount: DWORD;
    QueueDepth: DWORD;
    SplitCount: DWORD;
    QueryTime: LARGE_INTEGER;
    StorageDeviceNumber: DWORD;
    StorageManagerName: array [0..7] of WCHAR;
  end;
  DISK_PERFORMANCE = _DISK_PERFORMANCE;
  TDiskPerformance = DISK_PERFORMANCE;
  PDiskPerformance = PDISK_PERFORMANCE;

const
   //  Valid values for the bCommandReg member of IDEREGS.
  IDE_ATAPI_IDENTIFY = $A1;  //  Returns ID sector for ATAPI.
  IDE_ATA_IDENTIFY = $EC;  //  Returns ID sector for ATA.
  IDE_EXECUTE_SMART_FUNCTION = $B0;  //  Returns ID sector for SMART.

  IDENTIFY_BUFFER_SIZE       = 512;

  NUM_ATTRIBUTE_STRUCTS = 30;

  SMART_GET_VERSION = $00074080;  // CTL_CODE(IOCTL_DISK_BASE, $0020, METHOD_BUFFERED, FILE_READ_ACCESS);
  SMART_SEND_DRIVE_COMMAND = $0007c084; // CTL_CODE(IOCTL_DISK_BASE, $0021, METHOD_BUFFERED, FILE_READ_ACCESS or FILE_WRITE_ACCESS);
  SMART_RCV_DRIVE_DATA = $0007c088; // CTL_CODE(IOCTL_DISK_BASE, $0022, METHOD_BUFFERED, FILE_READ_ACCESS or FILE_WRITE_ACCESS);

  READ_ATTRIBUTE_BUFFER_SIZE = 512;
  READ_THRESHOLD_BUFFER_SIZE = 512;

  SMART_READ_ATTRIBUTE_VALUES = $D0;	// ATA4: Renamed SMART READ DATA
  SMART_READ_ATTRIBUTE_THRESHOLDS = $D1;	// Obsoleted in ATA4!
  SMART_ENABLE_DISABLE_ATTRIBUTE_AUTOSAVE = $D2;
  SMART_SAVE_ATTRIBUTE_VALUES = $D3;
  SMART_EXECUTE_OFFLINE_IMMEDIATE = $D4;	// ATA4
// Vendor specific commands:
  SMART_ENABLE_SMART_OPERATIONS = $D8;
  SMART_DISABLE_SMART_OPERATIONS = $D9;
  SMART_RETURN_SMART_STATUS = $DA;


  SMART_CYL_LOW = $4F;
  SMART_CYL_HI = $C2;

  ATTR_INVALID = 0;
  ATTR_READ_ERROR_RATE = 1;
  ATTR_THROUGHPUT_PERF = 2;
  ATTR_SPIN_UP_TIME = 3;
  ATTR_START_STOP_COUNT = 4;
  ATTR_REALLOC_SECTOR_COUNT = 5;
  ATTR_READ_CHANNEL_MARGIN = 6;
  ATTR_SEEK_ERROR_RATE = 7;
  ATTR_SEEK_TIME_PERF = 8;
  ATTR_POWER_ON_HRS_COUNT = 9;
  ATTR_SPIN_RETRY_COUNT = 10;
  ATTR_CALIBRATION_RETRY_COUNT = 11;
  ATTR_POWER_CYCLE_COUNT = 12;
  ATTR_SOFT_READ_ERROR_RATE = 13;
  ATTR_AIR_FLOW_TEMPERATURE = 190;
  ATTR_G_SENSE_ERROR_RATE = 191;
  ATTR_POWER_OFF_RETRACT_CYCLE = 192;
  ATTR_LOAD_UNLOAD_CYCLE_COUNT = 193;
  ATTR_TEMPERATURE = 194;
  ATTR_REALLOCATION_EVENTS_COUNT = 196;
  ATTR_CURRENT_PENDING_SECTOR_COUNT = 197;
  ATTR_UNCORRECTABLE_SECTOR_COUNT = 198;
  ATTR_ULTRADMA_CRC_ERROR_RATE = 199;
  ATTR_WRITE_ERROR_RATE = 200;
  ATTR_DISK_SHIFT = 220;
  ATTR_G_SENSE_ERROR_RATEII = 221;
  ATTR_LOADED_HOURS = 222;
  ATTR_LOAD_UNLOAD_RETRY_COUNT = 223;
  ATTR_LOAD_FRICTION = 224;
  ATTR_LOAD_UNLOAD_CYCLE_COUNTII = 225;
  ATTR_LOAD_IN_TIME = 226;
  ATTR_TORQUE_AMPLIFICATION_COUNT = 227;
  ATTR_POWER_OFF_RETRACT_COUNT = 228;
  ATTR_GMR_HEAD_AMPLITUDE = 230;
  ATTR_TEMPERATUREII = 231;
  ATTR_READ_ERROR_RETRY_RATE = 250;


  METHOD_BUFFERED   = 0;

  FILE_DEVICE_BEEP                = $00000001;
  FILE_DEVICE_CD_ROM              = $00000002;
  FILE_DEVICE_CD_ROM_FILE_SYSTEM  = $00000003;
  FILE_DEVICE_CONTROLLER          = $00000004;
  FILE_DEVICE_DATALINK            = $00000005;
  FILE_DEVICE_DFS                 = $00000006;
  FILE_DEVICE_DISK                = $00000007;
  FILE_DEVICE_DISK_FILE_SYSTEM    = $00000008;
  FILE_DEVICE_FILE_SYSTEM         = $00000009;
  FILE_DEVICE_INPORT_PORT         = $0000000a;
  FILE_DEVICE_KEYBOARD            = $0000000b;
  FILE_DEVICE_MAILSLOT            = $0000000c;
  FILE_DEVICE_MIDI_IN             = $0000000d;
  FILE_DEVICE_MIDI_OUT            = $0000000e;
  FILE_DEVICE_MOUSE               = $0000000f;
  FILE_DEVICE_MULTI_UNC_PROVIDER  = $00000010;
  FILE_DEVICE_NAMED_PIPE          = $00000011;
  FILE_DEVICE_NETWORK             = $00000012;
  FILE_DEVICE_NETWORK_BROWSER     = $00000013;
  FILE_DEVICE_NETWORK_FILE_SYSTEM = $00000014;
  FILE_DEVICE_NULL                = $00000015;
  FILE_DEVICE_PARALLEL_PORT       = $00000016;
  FILE_DEVICE_PHYSICAL_NETCARD    = $00000017;
  FILE_DEVICE_PRINTER             = $00000018;
  FILE_DEVICE_SCANNER             = $00000019;
  FILE_DEVICE_SERIAL_MOUSE_PORT   = $0000001a;
  FILE_DEVICE_SERIAL_PORT         = $0000001b;
  FILE_DEVICE_SCREEN              = $0000001c;
  FILE_DEVICE_SOUND               = $0000001d;
  FILE_DEVICE_STREAMS             = $0000001e;
  FILE_DEVICE_TAPE                = $0000001f;
  FILE_DEVICE_TAPE_FILE_SYSTEM    = $00000020;
  FILE_DEVICE_TRANSPORT           = $00000021;
  FILE_DEVICE_UNKNOWN             = $00000022;
  FILE_DEVICE_VIDEO               = $00000023;
  FILE_DEVICE_VIRTUAL_DISK        = $00000024;
  FILE_DEVICE_WAVE_IN             = $00000025;
  FILE_DEVICE_WAVE_OUT            = $00000026;
  FILE_DEVICE_8042_PORT           = $00000027;
  FILE_DEVICE_NETWORK_REDIRECTOR  = $00000028;
  FILE_DEVICE_BATTERY             = $00000029;
  FILE_DEVICE_BUS_EXTENDER        = $0000002a;
  FILE_DEVICE_MODEM               = $0000002b;
  FILE_DEVICE_VDM                 = $0000002c;
  FILE_DEVICE_MASS_STORAGE        = $0000002d;
  FILE_DEVICE_SMB                 = $0000002e;
  FILE_DEVICE_KS                  = $0000002f;
  FILE_DEVICE_CHANGER             = $00000030;
  FILE_DEVICE_SMARTCARD           = $00000031;
  FILE_DEVICE_ACPI                = $00000032;
  FILE_DEVICE_DVD                 = $00000033;
  FILE_DEVICE_FULLSCREEN_VIDEO    = $00000034;
  FILE_DEVICE_DFS_FILE_SYSTEM     = $00000035;
  FILE_DEVICE_DFS_VOLUME          = $00000036;
  FILE_DEVICE_SERENUM             = $00000037;
  FILE_DEVICE_TERMSRV             = $00000038;
  FILE_DEVICE_KSEC                = $00000039;
  FILE_DEVICE_FIPS                = $0000003A;
  FILE_DEVICE_INFINIBAND          = $0000003B;

  FILE_ANY_ACCESS     = 0;
  FILE_SPECIAL_ACCESS = FILE_ANY_ACCESS;
  FILE_READ_ACCESS    = $0001;           // file & pipe
  FILE_WRITE_ACCESS   = $0002;           // file & pipe

  SCSI_IOCTL_DATA_OUT =         0;
  SCSI_IOCTL_DATA_IN  =         1;
  SCSI_IOCTL_DATA_UNSPECIFIED = 2;

  CDB_INQUIRY_EVPD = 1;

  SCSIOP_INQUIRY = $12;
  SCSIOP_MODE_SENSE = $1A;

  MODE_PAGE_CAPABILITIES = $2A;

  CDB6GENERIC_LENGTH = 6;

  FILE_DEVICE_SCSI = $0000001b;

  IOCTL_SCSI_BASE = FILE_DEVICE_CONTROLLER;
  IOCTL_STORAGE_BASE = FILE_DEVICE_MASS_STORAGE;
  IOCTL_SCSI_PASS_THROUGH = ((IOCTL_SCSI_BASE shl 16) or ($0401 shl 2) or METHOD_BUFFERED or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14));
  IOCTL_SCSI_GET_INQUIRY_DATA = ((IOCTL_SCSI_BASE shl 16) or ($0403 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14));
  IOCTL_SCSI_GET_CAPABILITIES = ((IOCTL_SCSI_BASE shl 16) or ($0404 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14));
  IOCTL_SCSI_PASS_THROUGH_DIRECT = ((IOCTL_SCSI_BASE shl 16) or ($0405 shl 2) or METHOD_BUFFERED or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14)); //$4D014
  IOCTL_SCSI_GET_ADDRESS = ((IOCTL_SCSI_BASE shl 16) or ($0406 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14));
  IOCTL_SCSI_RESCAN_BUS = ((IOCTL_SCSI_BASE shl 16) or ($0407 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14));
  IOCTL_SCSI_GET_DUMP_POINTERS = ((IOCTL_SCSI_BASE shl 16) or ($0408 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14));

  IOCTL_SCSI_MINIPORT = ((IOCTL_SCSI_BASE shl 16) or ($0402 shl 2) or METHOD_BUFFERED or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14)); //$0004D008
  IOCTL_SCSI_MINIPORT_SMART_VERSION              = (FILE_DEVICE_SCSI shl 16) or $0500;
  IOCTL_SCSI_MINIPORT_IDENTIFY                    = (FILE_DEVICE_SCSI shl 16) or $0501;
  IOCTL_SCSI_MINIPORT_READ_SMART_ATTRIBS          = (FILE_DEVICE_SCSI shl 16) or $0502;
  IOCTL_SCSI_MINIPORT_READ_SMART_THRESHOLDS      = (FILE_DEVICE_SCSI shl 16) or $0503;
  IOCTL_SCSI_MINIPORT_ENABLE_SMART                = (FILE_DEVICE_SCSI shl 16) or $0504;
  IOCTL_SCSI_MINIPORT_DISABLE_SMART              = (FILE_DEVICE_SCSI shl 16) or $0505;
  IOCTL_SCSI_MINIPORT_RETURN_STATUS              = (FILE_DEVICE_SCSI shl 16) or $0506;
  IOCTL_SCSI_MINIPORT_ENABLE_DISABLE_AUTOSAVE    = (FILE_DEVICE_SCSI shl 16) or $0507;
  IOCTL_SCSI_MINIPORT_SAVE_ATTRIBUTE_VALUES      = (FILE_DEVICE_SCSI shl 16) or $0508;
  IOCTL_SCSI_MINIPORT_EXECUTE_OFFLINE_DIAGS      = (FILE_DEVICE_SCSI shl 16) or $0509;
  IOCTL_SCSI_MINIPORT_ENABLE_DISABLE_AUTO_OFFLINE = (FILE_DEVICE_SCSI shl 16) or $050a;

  IOCTL_ATA_PASS_THROUGH = (IOCTL_SCSI_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($040B shl 2) or (METHOD_BUFFERED);
  IOCTL_ATA_PASS_THROUGH_DIRECT = (IOCTL_SCSI_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($040C shl 2) or (METHOD_BUFFERED);

  IOCTL_DISK_BASE = FILE_DEVICE_DISK;
  IOCTL_DISK_GET_DRIVE_GEOMETRY = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0000 shl 2) or METHOD_BUFFERED);
  IOCTL_DISK_GET_PARTITION_INFO = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0001 shl 2) or METHOD_BUFFERED);
  IOCTL_DISK_GET_DRIVE_LAYOUT = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0003 shl 2) or METHOD_BUFFERED);
  IOCTL_VOLUME_BASE = Cardinal('V');
  IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS = (
    (IOCTL_VOLUME_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    (0 shl 2) or METHOD_BUFFERED);
  IOCTL_DISK_CONTROLLER_NUMBER = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0011 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_GET_MEDIA_TYPES_EX = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0301 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_GET_MEDIA_SERIAL_NUMBER = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0304 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_QUERY_PROPERTY = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0500 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_GET_DEVICE_NUMBER = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0420 shl 2) or METHOD_BUFFERED);
  IOCTL_BATTERY_QUERY_INFORMATION = (FILE_DEVICE_BATTERY shl 16) or (FILE_READ_ACCESS shl 14) or ($11 shl 2) or METHOD_BUFFERED;
  IOCTL_BATTERY_QUERY_TAG = (FILE_DEVICE_BATTERY shl 16) or (FILE_READ_ACCESS shl 14) or ($10 shl 2) or METHOD_BUFFERED;
  IOCTL_BATTERY_QUERY_STATUS = (FILE_DEVICE_BATTERY shl 16) or (FILE_READ_ACCESS shl 14) or ($13 shl 2) or METHOD_BUFFERED;
  IOCTL_DISK_GET_LENGTH_INFO = ((IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or ($0017 shl 2) or METHOD_BUFFERED);



  FSCTL_LOCK_VOLUME = ((FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or (6 shl 2) or METHOD_BUFFERED);

  FSCTL_UNLOCK_VOLUME = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (7 shl 2) or METHOD_BUFFERED);

  IOCTL_STORAGE_MEDIA_REMOVAL = ((IOCTL_STORAGE_BASE shl 16) or
                                     (FILE_READ_ACCESS shl 14) or
                                     ($0201 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_EJECT_MEDIA   = ((IOCTL_STORAGE_BASE shl 16) or
                                     (FILE_READ_ACCESS shl 14) or
                                     ($0202 shl 2) or METHOD_BUFFERED);

  FSCTL_DISMOUNT_VOLUME = ((FILE_DEVICE_FILE_SYSTEM shl 16) or
                                (FILE_ANY_ACCESS shl 14) or
                                (8 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_PERFORMANCE = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0008 shl 2) or METHOD_BUFFERED);

  ATA_FLAGS_DRDY_REQUIRED = $01;
  ATA_FLAGS_DATA_IN       = $02;
  ATA_FLAGS_DATA_OUT      = $04;
  ATA_FLAGS_48BIT_COMMAND = $08;
  ATA_FLAGS_USE_DMA       = $10;
  ATA_FLAGS_NO_MULTIPLE   = $20;


  VWIN32_DIOC_DOS_IOCTL21 = 1; // INT 21h - 4400h, 4411h
  VWIN32_DIOC_DOS_IOCTL25 = 2; // INT 25h - Disk Read
  VWIN32_DIOC_DOS_IOCTL26 = 3; // INT 26h - Disk Write
  VWIN32_DIOC_DOS_IOCTL13 = 4; // INT 13h
  VWIN32_DIOC_DOS_DRIVEINFO = 6; // INT 21h - Function 730X commands

   //  Bits returned in the fCapabilities member of GETVERSIONOUTPARAMS
  CAP_IDE_ID_FUNCTION = 1;  // ATA ID command supported
  CAP_IDE_ATAPI_ID = 2;  // ATAPI ID command supported
  CAP_IDE_EXECUTE_SMART_FUNCTION = 4;  // SMART commannds supported

  DataSize = sizeof(TSendCmdInParams)-1+IDENTIFY_BUFFER_SIZE;
  BufferSize = SizeOf(SRB_IO_CONTROL)+DataSize;
  W9xBufferSize = IDENTIFY_BUFFER_SIZE+16;

  PARTITION_ENTRY_UNUSED    = $00; // Entry unused
  PARTITION_FAT_12          = $01; // 12-bit FAT entries
  PARTITION_XENIX_1         = $02; // Xenix
  PARTITION_XENIX_2         = $03; // Xenix
  PARTITION_FAT_16          = $04; // 16-bit FAT entries
  PARTITION_EXTENDED        = $05; // Extended partition entry
  PARTITION_HUGE            = $06; // Huge partition MS-DOS V4
  PARTITION_IFS             = $07; // IFS Partition
  PARTITION_OS2BOOTMGR      = $0A; // OS/2 Boot Manager/OPUS/Coherent swap
  PARTITION_FAT32           = $0B; // FAT32
  PARTITION_FAT32_XINT13    = $0C; // FAT32 using extended int13 services
  PARTITION_XINT13          = $0E; // Win95 partition using extended int13 services
  PARTITION_XINT13_EXTENDED = $0F; // Same as type 5 but uses extended int13 services
  PARTITION_PREP            = $41; // PowerPC Reference Platform (PReP) Boot Partition
  PARTITION_LDM             = $42; // Logical Disk Manager partition
  PARTITION_UNIX            = $63; // Unix
  VALID_NTFT                = $C0; // NTFT uses high order bits

  PARTITION_LINUXSWAP       = $82;  // Linux Swap
  PARTITION_REISER          = $83;  // Linux Ext2, Ext3 or ReiserFS
  PARTITION_LINUXEXT        = $85;  // Linux Entended

// The high bit of the partition type code indicates that a partition
// is part of an NTFT mirror or striped array.
  PARTITION_NTFT = $80; // NTFT partition

  bBytesPerSector = 512;

  Unknown = 0;                // Format is unknown
  F5_1Pt2_512 = 1;            // 5.25", 1.2MB,  512 bytes/sector
  F3_1Pt44_512 = 2;           // 3.5",  1.44MB, 512 bytes/sector
  F3_2Pt88_512 = 3;           // 3.5",  2.88MB, 512 bytes/sector
  F3_20Pt8_512 = 4;           // 3.5",  20.8MB, 512 bytes/sector
  F3_720_512 = 5;             // 3.5",  720KB,  512 bytes/sector
  F5_360_512 = 6;             // 5.25", 360KB,  512 bytes/sector
  F5_320_512 = 7;             // 5.25", 320KB,  512 bytes/sector
  F5_320_1024 = 8;            // 5.25", 320KB,  1024 bytes/sector
  F5_180_512 = 9;             // 5.25", 180KB,  512 bytes/sector
  F5_160_512 = $A;             // 5.25", 160KB,  512 bytes/sector
  RemovableMedia = $B;         // Removable media other than floppy
  FixedMedia = $C;             // Fixed hard disk media
  F3_120M_512 = $D;            // 3.5", 120M Floppy
  F3_640_512 = $E;             // 3.5" ,  640KB,  512 bytes/sector
  F5_640_512 = $F;             // 5.25",  640KB,  512 bytes/sector
  F5_720_512 = $10;             // 5.25",  720KB,  512 bytes/sector
  F3_1Pt2_512 = $11;            // 3.5" ,  1.2Mb,  512 bytes/sector
  F3_1Pt23_1024 = $12;          // 3.5" ,  1.23Mb, 1024 bytes/sector
  F5_1Pt23_1024 = $13;          // 5.25",  1.23MB, 1024 bytes/sector
  F3_128Mb_512 = $14;           // 3.5" MO 128Mb   512 bytes/sector
  F3_230Mb_512 = $15;           // 3.5" MO 230Mb   512 bytes/sector
  F8_256_128 = $16;             // 8",     256KB,  128 bytes/sector
  F3_200Mb_512 = $17;           // 3.5",   200M Floppy (HiFD)
  DDS_4mm            = $20;  // Tape - DAT DDS1,2,... (all vendors)
  MiniQic            = $21;  // Tape - miniQIC Tape
  Travan             = $22;  // Tape - Travan TR-1,2,3,...
  QIC                = $23;  // Tape - QIC
  MP_8mm             = $24;  // Tape - 8mm Exabyte Metal Particle
  AME_8mm            = $25;  // Tape - 8mm Exabyte Advanced Metal Evap
  AIT1_8mm           = $26;  // Tape - 8mm Sony AIT
  DLT                = $27;  // Tape - DLT Compact IIIxt, IV
  NCTP               = $28;  // Tape - Philips NCTP
  IBM_3480           = $29;  // Tape - IBM 3480
  IBM_3490E          = $2A;  // Tape - IBM 3490E
  IBM_Magstar_3590   = $2B;  // Tape - IBM Magstar 3590
  IBM_Magstar_MP     = $2C;  // Tape - IBM Magstar MP
  STK_DATA_D3        = $2D;  // Tape - STK Data D3
  SONY_DTF           = $2E;  // Tape - Sony DTF
  DV_6mm             = $2F;  // Tape - 6mm Digital Video
  DMI                = $30;  // Tape - Exabyte DMI and compatibles
  SONY_D2            = $31;  // Tape - Sony D2S and D2L
  CLEANER_CARTRIDGE  = $32;  // Cleaner - All Drive types that support Drive Cleaners
  CD_ROM             = $33;  // Optical disk - CD
  CD_R               = $34;  // Optical disk - CD-Recordable (Write Once)
  CD_RW              = $35;  // Optical disk - CD-Rewriteable
  DVD_ROM            = $36;  // Optical disk - DVD-ROM
  DVD_R              = $37;  // Optical disk - DVD-Recordable (Write Once)
  DVD_RW             = $38;  // Optical disk - DVD-Rewriteable
  MO_3_RW            = $39;  // Optical disk - 3.5" Rewriteable MO Disk
  MO_5_WO            = $3A;  // Optical disk - MO 5.25" Write Once
  MO_5_RW            = $3B;  // Optical disk - MO 5.25" Rewriteable (not LIMDOW)
  MO_5_LIMDOW        = $3C;  // Optical disk - MO 5.25" Rewriteable (LIMDOW)
  PC_5_WO            = $3D;  // Optical disk - Phase Change 5.25" Write Once Optical
  PC_5_RW            = $3E;  // Optical disk - Phase Change 5.25" Rewriteable
  PD_5_RW            = $3F;  // Optical disk - PhaseChange Dual Rewriteable
  ABL_5_WO           = $40;  // Optical disk - Ablative 5.25" Write Once Optical
  PINNACLE_APEX_5_RW = $41;  // Optical disk - Pinnacle Apex 4.6GB Rewriteable Optical
  SONY_12_WO         = $42;  // Optical disk - Sony 12" Write Once
  PHILIPS_12_WO      = $43;  // Optical disk - Philips/LMS 12" Write Once
  HITACHI_12_WO      = $44;  // Optical disk - Hitachi 12" Write Once
  CYGNET_12_WO       = $45;  // Optical disk - Cygnet/ATG 12" Write Once
  KODAK_14_WO        = $46;  // Optical disk - Kodak 14" Write Once
  MO_NFR_525         = $47;  // Optical disk - Near Field Recording (Terastor)
  NIKON_12_RW        = $48;  // Optical disk - Nikon 12" Rewriteable
  IOMEGA_ZIP         = $49;  // Magnetic disk - Iomega Zip
  IOMEGA_JAZ         = $4A;  // Magnetic disk - Iomega Jaz
  SYQUEST_EZ135      = $4B;  // Magnetic disk - Syquest EZ135
  SYQUEST_EZFLYER    = $4C;  // Magnetic disk - Syquest EzFlyer
  SYQUEST_SYJET      = $4D;  // Magnetic disk - Syquest SyJet
  AVATAR_F2          = $4E;  // Magnetic disk - 2.5" Floppy
  MP2_8mm            = $4F;  // Tape - 8mm Hitachi
  DST_S              = $50;  // Ampex DST Small Tapes
  DST_M              = $51;  // Ampex DST Medium Tapes
  DST_L              = $52;  // Ampex DST Large Tapes
  VXATape_1          = $53;  // Ecrix 8mm Tape
  VXATape_2          = $54;  // Ecrix 8mm Tape
  STK_9840           = $55;  // STK 9840
  LTO_Ultrium        = $56;  // IBM, HP, Seagate LTO Ultrium
  LTO_Accelis        = $57;  // IBM, HP, Seagate LTO Accelis
  DVD_RAM            = $58;  // Optical disk - DVD-RAM
  AIT_8mm            = $59;  // AIT2 or higher
  ADR_1              = $5A;  // OnStream ADR Mediatypes
  ADR_2              = $5B;
  STK_9940           = $5C;  // STK 9940

  BusTypeUnknown = $00;
  BusTypeScsi = $01;
  BusTypeAtapi = $02;
  BusTypeAta = $03;
  BusType1394 = $04;
  BusTypeSsa = $05;
  BusTypeFibre = $06;
  BusTypeUsb = $07;
  BusTypeRAID = $08;
  BusTypeiSCSI= $09;
  BusTypeSas = $0A;
  BusTypeSata = $0B;
  BusTypeSd = $0C;
  BusTypeMmc = $0D;
  BusTypeVirtual = $0E;
  BusTypeFileBackedVirtual = $0F;
  BusTypeMaxReserved = $7F;

function CTL_CODE(DeviceType, Func, Method, Access: WORD): DWORD;  

function IsRecognizedPartition(PartitionType: Cardinal): Boolean;
function IsContainerPartition(PartitionType: Cardinal): Boolean;
function IsFTPartition(PartitionType: Cardinal): Boolean;
function GetPartitionSystem(PartitionType: Cardinal): string;
function GetPartitionType(PartitionNumber,PartitionType: Cardinal): string;

function ReadPhysicalSector(Drive: String; StartSector: Cardinal; SectorsToRead: byte; Dump: Pointer): Integer;
function GetDeviceTypeStr(AType: Cardinal): string;
function GetDeviceMediaTypeStr(AMT: Cardinal): string;
function GetStorageBusTypeStr(ABT: Cardinal; AEmptyUnknown: boolean = False): string;

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry, System.Math,
     {$ELSE}
     Registry, Math,
     {$ENDIF}
     MiTeC_Routines;

function CTL_CODE(DeviceType, Func, Method, Access: WORD): DWORD;
begin
  Result := (DeviceType shl 16) or (Access shl 14) or (Func shl 2) or Method;
end;

procedure BigEndian(const Source; var Dest; Count: integer);
var
  pSrc,pDst: PAnsiChar;
  i: integer;
begin
  pSrc:=@Source;
  pDst:=PAnsiChar(@Dest)+Count;
  for i:=0 to Count-1 do begin
    Dec(pDst);
    pDst^:=pSrc^;
    Inc(pSrc);
  end;
end;

function ByteArrayToStr(Buffer: array of byte): string;
var
  i: Integer;
begin
  Result:='';
  for i:=0 to High(Buffer) do
    Result:=Result+Chr(Buffer[i]);
end;

procedure ChangeByteOrder( var Data; Size : Integer );
var
  ptr: PAnsiChar;
  i: Integer;
  c: AnsiChar;
begin
  ptr:=@Data;
  for i:=0 to (Size shr 1)-1 do begin
    c:=ptr^;
    ptr^:=(ptr+1)^;
    (ptr+1)^:=c;
    Inc(ptr,2);
  end;
end;

function IsRecognizedPartition(PartitionType: Cardinal): Boolean;
begin
  Result :=
    (((PartitionType and PARTITION_NTFT) <> 0) and ((PartitionType and not $C0) = PARTITION_FAT_12)) or
    (((PartitionType and PARTITION_NTFT) <> 0) and ((PartitionType and not $C0) = PARTITION_IFS)) or
    (((PartitionType and PARTITION_NTFT) <> 0) and ((PartitionType and not $C0) = PARTITION_HUGE)) or
    (((PartitionType and PARTITION_NTFT) <> 0) and ((PartitionType and not $C0) = PARTITION_FAT32)) or
    (((PartitionType and PARTITION_NTFT) <> 0) and ((PartitionType and not $C0) = PARTITION_FAT32_XINT13)) or
    (((PartitionType and PARTITION_NTFT) <> 0) and ((PartitionType and not $C0) = PARTITION_XINT13)) or
    ((PartitionType) = PARTITION_FAT_12) or
    ((PartitionType) = PARTITION_FAT_16) or
    ((PartitionType) = PARTITION_IFS) or
    ((PartitionType) = PARTITION_HUGE) or
    ((PartitionType) = PARTITION_FAT32) or
    ((PartitionType) = PARTITION_FAT32_XINT13) or
    ((PartitionType) = PARTITION_XINT13);
end;

function IsContainerPartition(PartitionType: Cardinal): Boolean;
begin
  Result :=
    (PartitionType = PARTITION_EXTENDED) or
    (PartitionType = PARTITION_XINT13_EXTENDED);
end;

function IsFTPartition(PartitionType: Cardinal): Boolean;
begin
  Result := ((PartitionType and PARTITION_NTFT) = PARTITION_NTFT) and IsRecognizedPartition(PartitionType);
end;

function GetPartitionSystem(PartitionType: Cardinal): string;
begin
  case PartitionType of
    PARTITION_FAT_12: Result:='FAT12';
    PARTITION_FAT_16,
    PARTITION_HUGE: Result:='FAT16';
    PARTITION_FAT32,
    PARTITION_FAT32_XINT13,
    PARTITION_XINT13,
    PARTITION_XINT13_EXTENDED: Result:='FAT32';
    PARTITION_IFS: Result:='NTFS';
    PARTITION_OS2BOOTMGR: Result:='HPFS';
    PARTITION_XENIX_1,
    PARTITION_XENIX_2: Result:='Xenix';
    PARTITION_UNIX: Result:='Unix';
    PARTITION_LINUXSWAP: Result:='Linux Swap';
    PARTITION_REISER: Result:='Linux Ext2, Ext3 or ReiserFS';
    PARTITION_LINUXEXT: Result:='Linux Entended';
    else Result:='Unknown';
  end;
end;

function GetPartitionType(PartitionNumber,PartitionType: Cardinal): string;
begin
  Result:='Primary';
  if IsContainerPartition(PartitionType) then
    Result:='Extended';
  if IsFTPartition(PartitionType) then
    Result:='FT';
  if not IsContainerPartition(PartitionType) and (PartitionNumber>1) then
    Result:='Logical';
end;

function GetDeviceMediaTypeStr(AMT: Cardinal): string;
begin
  Result:=Format('Unknown (0x%x)',[AMT]);
  case AMT of
    F5_1Pt2_512:Result:='5.25", 1.2MB,  512 bytes/sector';
    F3_1Pt44_512:Result:='3.5",  1.44MB, 512 bytes/sector';
    F3_2Pt88_512:Result:='3.5",  2.88MB, 512 bytes/sector';
    F3_20Pt8_512:Result:='3.5",  20.8MB, 512 bytes/sector';
    F3_720_512:Result:='3.5",  720KB,  512 bytes/sector';
    F5_360_512:Result:='5.25", 360KB,  512 bytes/sector';
    F5_320_512:Result:='5.25", 320KB,  512 bytes/sector';
    F5_320_1024:Result:='5.25", 320KB,  1024 bytes/sector';
    F5_180_512:Result:='5.25", 180KB,  512 bytes/sector';
    F5_160_512:Result:='5.25", 160KB,  512 bytes/sector';
    RemovableMedia:Result:='Removable media other than floppy';
    FixedMedia:Result:='Fixed hard disk media';
    F3_120M_512:Result:='3.5", 120M Floppy';
    F3_640_512:Result:='3.5" ,  640KB,  512 bytes/sector';
    F5_640_512:Result:='5.25",  640KB,  512 bytes/sector';
    F5_720_512:Result:='5.25",  720KB,  512 bytes/sector';
    F3_1Pt2_512:Result:='3.5" ,  1.2Mb,  512 bytes/sector';
    F3_1Pt23_1024:Result:='3.5" ,  1.23Mb, 1024 bytes/sector';
    F5_1Pt23_1024:Result:='5.25",  1.23MB, 1024 bytes/sector';
    F3_128Mb_512:Result:='3.5" MO 128Mb   512 bytes/sector';
    F3_230Mb_512:Result:='3.5" MO 230Mb   512 bytes/sector';
    F8_256_128:Result:='8",     256KB,  128 bytes/sector';
    F3_200Mb_512:Result:='3.5",   200M Floppy (HiFD)';
    DDS_4mm           :Result:='Tape - DAT DDS1,2,... (all vendors)';
    MiniQic           :Result:='Tape - miniQIC Tape';
    Travan            :Result:='Tape - Travan TR-1,2,3,...';
    QIC               :Result:='Tape - QIC';
    MP_8mm            :Result:='Tape - 8mm Exabyte Metal Particle';
    AME_8mm           :Result:='Tape - 8mm Exabyte Advanced Metal Evap';
    AIT1_8mm          :Result:='Tape - 8mm Sony AIT';
    DLT               :Result:='Tape - DLT Compact IIIxt, IV';
    NCTP              :Result:='Tape - Philips NCTP';
    IBM_3480          :Result:='Tape - IBM 3480';
    IBM_3490E         :Result:='Tape - IBM 3490E';
    IBM_Magstar_3590  :Result:='Tape - IBM Magstar 3590';
    IBM_Magstar_MP    :Result:='Tape - IBM Magstar MP';
    STK_DATA_D3       :Result:='Tape - STK Data D3';
    SONY_DTF          :Result:='Tape - Sony DTF';
    DV_6mm            :Result:='Tape - 6mm Digital Video';
    DMI               :Result:='Tape - Exabyte DMI and compatibles';
    SONY_D2           :Result:='Tape - Sony D2S and D2L';
    CLEANER_CARTRIDGE :Result:='Cleaner - All Drive types that support Drive Cleaners';
    CD_ROM            :Result:='Optical disk - CD';
    CD_R              :Result:='Optical disk - CD-Recordable (Write Once)';
    CD_RW             :Result:='Optical disk - CD-Rewriteable';
    DVD_ROM           :Result:='Optical disk - DVD-ROM';
    DVD_R             :Result:='Optical disk - DVD-Recordable (Write Once)';
    DVD_RW            :Result:='Optical disk - DVD-Rewriteable';
    MO_3_RW           :Result:='Optical disk - 3.5" Rewriteable MO Disk';
    MO_5_WO           :Result:='Optical disk - MO 5.25" Write Once';
    MO_5_RW           :Result:='Optical disk - MO 5.25" Rewriteable (not LIMDOW)';
    MO_5_LIMDOW       :Result:='Optical disk - MO 5.25" Rewriteable (LIMDOW)';
    PC_5_WO           :Result:='Optical disk - Phase Change 5.25" Write Once Optical';
    PC_5_RW           :Result:='Optical disk - Phase Change 5.25" Rewriteable';
    PD_5_RW           :Result:='Optical disk - PhaseChange Dual Rewriteable';
    ABL_5_WO          :Result:='Optical disk - Ablative 5.25" Write Once Optical';
    PINNACLE_APEX_5_RW:Result:='Optical disk - Pinnacle Apex 4.6GB Rewriteable Optical';
    SONY_12_WO        :Result:='Optical disk - Sony 12" Write Once';
    PHILIPS_12_WO     :Result:='Optical disk - Philips/LMS 12" Write Once';
    HITACHI_12_WO     :Result:='Optical disk - Hitachi 12" Write Once';
    CYGNET_12_WO      :Result:='Optical disk - Cygnet/ATG 12" Write Once';
    KODAK_14_WO       :Result:='Optical disk - Kodak 14" Write Once';
    MO_NFR_525        :Result:='Optical disk - Near Field Recording (Terastor)';
    NIKON_12_RW       :Result:='Optical disk - Nikon 12" Rewriteable';
    IOMEGA_ZIP        :Result:='Magnetic disk - Iomega Zip';
    IOMEGA_JAZ        :Result:='Magnetic disk - Iomega Jaz';
    SYQUEST_EZ135     :Result:='Magnetic disk - Syquest EZ135';
    SYQUEST_EZFLYER   :Result:='Magnetic disk - Syquest EzFlyer';
    SYQUEST_SYJET     :Result:='Magnetic disk - Syquest SyJet';
    AVATAR_F2         :Result:='Magnetic disk - 2.5" Floppy';
    MP2_8mm           :Result:='Tape - 8mm Hitachi';
    DST_S             :Result:='Ampex DST Small Tapes';
    DST_M             :Result:='Ampex DST Medium Tapes';
    DST_L             :Result:='Ampex DST Large Tapes';
    VXATape_1         :Result:='Ecrix 8mm Tape';
    VXATape_2         :Result:='Ecrix 8mm Tape';
    STK_9840          :Result:='STK 9840';
    LTO_Ultrium       :Result:='IBM, HP, Seagate LTO Ultrium';
    LTO_Accelis       :Result:='IBM, HP, Seagate LTO Accelis';
    DVD_RAM           :Result:='Optical disk - DVD-RAM';
    AIT_8mm           :Result:='AIT2 or higher';
    ADR_1             :Result:='OnStream ADR Mediatypes';
  //  ADR_2             :Result:='$5B';
    STK_9940          :Result:='STK 9940';
  end;
end;

function GetStorageBusTypeStr(ABT: Cardinal; AEmptyUnknown: boolean = False): string;
begin
  if AEmptyUnknown then
    Result:=''
  else
    Result:=Format('Unknown (0x%x)',[ABT]);
  case ABT of
    BusTypeScsi :Result:='SCSI';
    BusTypeAtapi :Result:='ATAPI';
    BusTypeAta :Result:='ATA';
    BusType1394 :Result:='IEEE 1394 (FireWire)';
    BusTypeSsa :Result:='SSA';
    BusTypeFibre :Result:='Fibre channel';
    BusTypeUsb :Result:='USB';
    BusTypeRAID :Result:='RAID';
    BusTypeiSCSI :Result:='iSCSI';
    BusTypeSas :Result:='Serial SCSI';
    BusTypeSata :Result:='SATA';
    BusTypeSd :Result:='SD';//SecureDigital
    BusTypeMmc :Result:='MMC';//MultiMediaCard
    BusTypeVirtual :Result:='Virtual';
    BusTypeFileBackedVirtual :Result:='File-backed Virtual';
    $11: Result:='NVMe'; //Non-Volatile Memory express
    $12: Result:='SCM'; //NVDIMM-N
    $13 :Result:='UFS';
  end;
end;


function ReadPhysicalSector(Drive: String; StartSector: Cardinal; SectorsToRead: byte; Dump: Pointer): Integer;
var
  bytestoread, numread: Cardinal;
  dwpointer: Cardinal;
  hdevice: thandle;
  ldistancelow, ldistancehigh: Cardinal;
begin
  Result:=0;
  try
    StrToInt(Drive);
    hDevice:=CreateFile(PChar(Format('\\.\PhysicalDrive%s',[Drive])),
                      GENERIC_READ,
                      {FILE_SHARE_READ OR} FILE_SHARE_WRITE,
                      nil,
                      OPEN_EXISTING,
                      FILE_FLAG_WRITE_THROUGH,
                      0);
  except
    hDevice:=CreateFile(PChar(Format('\\.\%s:',[Drive])),
                      GENERIC_READ,
                      {FILE_SHARE_READ OR} FILE_SHARE_WRITE,
                      nil,
                      OPEN_EXISTING,
                      FILE_FLAG_WRITE_THROUGH,
                      0);
  end;
  if hDevice<>INVALID_HANDLE_VALUE then begin
    ldistanceLow:=Cardinal(StartSector shl 9);
    ldistanceHigh:=Cardinal(StartSector shr (32-9));
    dwpointer:=SetFilePointer(hdevice,ldistancelow,@ldistancehigh,FILE_BEGIN);
    if dwPointer<>$FFFFFFFF then begin
      bytestoread:=SectorsToRead*bBytesPerSector;
      if not ReadFile(hDevice,Dump^,bytestoread,numread,nil) then
        Result:=GetLastError;
    end;
    CloseHandle(hDevice);
  end else
    Result:=GetLastError;
end;

function GetDeviceTypeStr(AType: Cardinal): string;
begin
  case AType of
    FILE_DEVICE_BEEP                : Result:='Beep';
    FILE_DEVICE_CD_ROM              : Result:='CDROM';
    FILE_DEVICE_CD_ROM_FILE_SYSTEM  : Result:='CDROM File System';
    FILE_DEVICE_CONTROLLER          : Result:='Controller';
    FILE_DEVICE_DATALINK            : Result:='DataLink';
    FILE_DEVICE_DFS                 : Result:='DFS';
    FILE_DEVICE_DISK                : Result:='Disk';
    FILE_DEVICE_DISK_FILE_SYSTEM    : Result:='Disk File System';
    FILE_DEVICE_FILE_SYSTEM         : Result:='File System';
    FILE_DEVICE_INPORT_PORT         : Result:='InPort Port';
    FILE_DEVICE_KEYBOARD            : Result:='Keyboard';
    FILE_DEVICE_MAILSLOT            : Result:='MailSlot';
    FILE_DEVICE_MIDI_IN             : Result:='MIDI In';
    FILE_DEVICE_MIDI_OUT            : Result:='MIDI Out';
    FILE_DEVICE_MOUSE               : Result:='Mouse';
    FILE_DEVICE_MULTI_UNC_PROVIDER  : Result:='Multi UNC Provider';
    FILE_DEVICE_NAMED_PIPE          : Result:='Named Pipe';
    FILE_DEVICE_NETWORK             : Result:='Network';
    FILE_DEVICE_NETWORK_BROWSER     : Result:='Network Browser';
    FILE_DEVICE_NETWORK_FILE_SYSTEM : Result:='Network File System';
    FILE_DEVICE_NULL                : Result:='Null';
    FILE_DEVICE_PARALLEL_PORT       : Result:='Parallel Port';
    FILE_DEVICE_PHYSICAL_NETCARD    : Result:='Network card';
    FILE_DEVICE_PRINTER             : Result:='Printer';
    FILE_DEVICE_SCANNER             : Result:='Scanner';
    FILE_DEVICE_SERIAL_MOUSE_PORT   : Result:='Serial Mouse Port';
    FILE_DEVICE_SERIAL_PORT         : Result:='Serial Port';
    FILE_DEVICE_SCREEN              : Result:='Screen';
    FILE_DEVICE_SOUND               : Result:='Sound';
    FILE_DEVICE_STREAMS             : Result:='Streams';
    FILE_DEVICE_TAPE                : Result:='Tape';
    FILE_DEVICE_TAPE_FILE_SYSTEM    : Result:='Tape File System';
    FILE_DEVICE_TRANSPORT           : Result:='Transport';
    FILE_DEVICE_UNKNOWN             : Result:='Unknown';
    FILE_DEVICE_VIDEO               : Result:='Video';
    FILE_DEVICE_VIRTUAL_DISK        : Result:='Virtual Disk';
    FILE_DEVICE_WAVE_IN             : Result:='WAVE In';
    FILE_DEVICE_WAVE_OUT            : Result:='WAVE Out';
    FILE_DEVICE_8042_PORT           : Result:='8042 Port';
    FILE_DEVICE_NETWORK_REDIRECTOR  : Result:='Network Redirector';
    FILE_DEVICE_BATTERY             : Result:='Battery';
    FILE_DEVICE_BUS_EXTENDER        : Result:='Bus Extender';
    FILE_DEVICE_MODEM               : Result:='Modem';
    FILE_DEVICE_VDM                 : Result:='VDM';
    FILE_DEVICE_MASS_STORAGE        : Result:='Mass Storage';
    FILE_DEVICE_SMB                 : Result:='SMB';
    FILE_DEVICE_KS                  : Result:='KS';
    FILE_DEVICE_CHANGER             : Result:='Changer';
    FILE_DEVICE_SMARTCARD           : Result:='SmartCard';
    FILE_DEVICE_ACPI                : Result:='ACPI';
    FILE_DEVICE_DVD                 : Result:='DVD';
    FILE_DEVICE_FULLSCREEN_VIDEO    : Result:='Fullscreen Video';
    FILE_DEVICE_DFS_FILE_SYSTEM     : Result:='DFS File System';
    FILE_DEVICE_DFS_VOLUME          : Result:='DFS Volume';
    FILE_DEVICE_SERENUM             : Result:='SerEnum';
    FILE_DEVICE_TERMSRV             : Result:='Terminal Server';
    FILE_DEVICE_KSEC                : Result:='KSEC';
    FILE_DEVICE_FIPS                : Result:='FIPS';
    FILE_DEVICE_INFINIBAND          : Result:='InfiniBand';
  end;
end;

end.

