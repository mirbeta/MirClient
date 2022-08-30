 {*******************************************************}
{       MiTeC System Information Component Suite        }
{              SMBIOS Detection Part                    }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_SMBIOS;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MSI_Common, MSI_Defs, MSI_DMA;

const
  StorageFolderName = 'SMBIOS';

  strm_Proc = 'Processor_%d';
  strm_MemMod = 'MemMod_%d';
  strm_MemDev = 'MemDev_%d';
  strm_Port = 'Port_%d';
  strm_Slot = 'Slot_%d';
  strm_Cache = 'Cache_%d';
  strm_OBD = 'OBD_%d';
  strm_OBDX = 'OBDX_%d';
  strm_TempProbe = 'TempProbe_%d';
  strm_CoolDev = 'CoolDev_%d';
  strm_CurrProbe = 'CurrProbe_%d';
  strm_VoltProbe = 'VoltProbe_%d';
  strm_TPMD = 'TPMD_%d';
  strm_Bat = 'BAT_%d';

  Processor_StorageFolderName = 'ProcessorList';
  MemoryModule_StorageFolderName = 'MemoryModuleList';
  MemoryDevice_StorageFolderName = 'MemoryDeviceList';
  Port_StorageFolderName = 'PortList';
  SystemSlot_StorageFolderName = 'SystemSlotList';
  Cache_StorageFolderName = 'CacheList';
  OnBoardDevice_StorageFolderName = 'OnBoardDeviceList';
  OnBoardDeviceEx_StorageFolderName = 'OnBoardDeviceExList';
  TemperatureProbe_StorageFolderName = 'TemperatureProbeList';
  CoolingDevice_StorageFolderName = 'CoolingDeviceList';
  CurrentProbe_StorageFolderName = 'CurrentProbeList';
  VoltageProbe_StorageFolderName = 'VoltageProbeList';
  TPMDevice_StorageFolderName = 'TPMDeviceList';
  PortableBattery_StorageFolderName = 'PortableBatteryList';

  RomBiosDumpBase    = Cardinal($000C0000);
  RomBiosDumpBasePtr = Pointer(RomBiosDumpBase);
  RomBiosDumpEnd     = Cardinal($000FFFFF);
  RomBiosDumpEndPtr  = Pointer(RomBiosDumpEnd);
  RomBiosDumpSize    = Cardinal(RomBiosDumpEnd - RomBiosDumpBase + 1);
  RomBiosBlockSize   = MaxWord;

type
  PRomBiosDump = ^TRomBiosDump;
  TRomBiosDump = record
    ByteArray: array [0..RomBiosDumpSize - 1] of Byte;
  end;

const
  SMB_BIOSINFO     = 0;  // BIOS Information
  SMB_SYSINFO      = 1;  // System Information
  SMB_BASEINFO     = 2;  // Base Board Information
  SMB_SYSENC       = 3;  // System Enclosure or Chassis
  SMB_CPU          = 4;  // Processor Information
  SMB_MEMCTRL      = 5;  // Memory Controller Information
  SMB_MEMMOD       = 6;  // Memory Module Information
  SMB_CACHE        = 7;  // Cache Information
  SMB_PORTCON      = 8;  // Port Connector Information
  SMB_SLOTS        = 9;  // System Slots
  SMB_ONBOARD      = 10;  // On Board Devices Information
  SMB_OEMSTR       = 11;  // OEM Strings
  SMB_SYSCFG       = 12;  // System Configuration Options
  SMB_LANG         = 13;  // BIOS Language Information
  SMB_GRP          = 14;  // Group Associations
  SMB_EVENT        = 15;  // System Event Log
  SMB_PHYSMEM      = 16;  // Physical Memory Array
  SMB_MEMDEV       = 17;  // Memory Device
  SMB_MEMERR32     = 18;  // 32-bit Memory Error Information
  SMB_MEMMAP       = 19;  // Memory Array Mapped Address
  SMB_MEMDEVMAP    = 20;  // Memory Device Mapped Address
  SMB_POINTER      = 21;  // Built-in Pointing Device
  SMB_BATTERY      = 22;  // Portable Battery
  SMB_RESET        = 23;  // System Reset
  SMB_SECURITY     = 24;  // Hardware Security
  SMB_POWER        = 25;  // System Power Controls
  SMB_VOLTAGE      = 26;  // Voltage Probe
  SMB_COOL         = 27;  // Cooling Device
  SMB_TEMP         = 28;  // Tempature Probe
  SMB_CURRENT      = 29;  // Electrical Current Probe
  SMB_OOBREM       = 30;  // Out-of-Band Remote Access
  SMB_BIS          = 31;  // Boot Integrity Services (BIS) Entry Point
  SMB_SYSBOOT      = 32;  // System Boot Information
  SMB_MEMERR64     = 33;  // 64-bit Memory Error Information
  SMB_MGT          = 34;  // Management Device
  SMB_MGTCMP       = 35;  // Management Device Component
  SMB_MGTTHR       = 36;  // Management Device Threshold Data
  SMB_MEMCHAN      = 37;  // Memory Channel
  SMB_IPMI         = 38;  // IPMI Device Information
  SMB_SPS          = 39;  // System Power Supply
  SMB_ADD          = 40;  // Additional  Information
  SMB_ONBOARDX     = 41;  // On Board Devices Extended Information
  SMB_MGMTCTRL     = 42;  // Management Controller Host Interface
  SMB_TPMDEV       = 43;  // TPM Device

  SMB_INACTIVE     = 126;  // Inactive
  SMB_EOT          = 127;  // End-of-Table
  SMBIOS_OEM_BEGIN = 128;
  SMBIOS_OEM_END   = 255;

  SMB_ACER_170     = 170;  //Acer Hotkey Function

  SMB_HP_204       = 204;  //HP ProLiant System/Rack Locator
  SMB_HP_209       = 209;  //HP BIOS PXE NIC PCI and MAC Information
  SMB_HP_212       = 212;  //HP 64-bit CRU Information
  SMB_HP_219       = 219;  //HP ProLiant Information
  SMB_HP_221       = 221;  //HP BIOS iSCSI NIC PCI and MAC Information
  SMB_HP_233       = 233;  //HP BIOS PXE NIC PCI and MAC Information

  SMB_IBM_131      = 131; //ThinkVantage Technologies feature bits
  SMB_IBM_135      = 135; //Device Presence Detection bits
  SMB_IBM_140      = 140; //ThinkPad Embedded Controller Program

  SMB_TableTypes: array[0..55] of record Typ: Byte; Name: string; end = (
    (Typ: SMB_BIOSINFO; Name: 'BIOS Information'),
    (Typ: SMB_SYSINFO; Name: 'System Information'),
    (Typ: SMB_BASEINFO; Name: 'Base Board Information'),
    (Typ: SMB_SYSENC; Name: 'System Enclosure or Chassis'),
    (Typ: SMB_CPU; Name: 'Processor Information'),
    (Typ: SMB_MEMCTRL; Name: 'Memory Controller Information'),
    (Typ: SMB_MEMMOD; Name: 'Memory Module Information'),
    (Typ: SMB_CACHE; Name: 'Cache Information'),
    (Typ: SMB_PORTCON; Name: 'Port Connector Information'),
    (Typ: SMB_SLOTS; Name: 'System Slots'),
    (Typ: SMB_ONBOARD; Name: 'On Board Devices Information'),
    (Typ: SMB_OEMSTR; Name: 'OEM Strings'),
    (Typ: SMB_SYSCFG; Name: 'System Configuration Options'),
    (Typ: SMB_LANG; Name: 'BIOS Language Information'),
    (Typ: SMB_GRP; Name: 'Group Associations'),
    (Typ: SMB_EVENT; Name: 'System Event Log'),
    (Typ: SMB_PHYSMEM; Name: 'Physical Memory Array'),
    (Typ: SMB_MEMDEV; Name: 'Memory Device'),
    (Typ: SMB_MEMERR32; Name: '32-bit Memory Error Information'),
    (Typ: SMB_MEMMAP; Name: 'Memory Array Mapped Address'),
    (Typ: SMB_MEMDEVMAP; Name: 'Memory Device Mapped Address'),
    (Typ: SMB_POINTER; Name: 'Built-in Pointing Device'),
    (Typ: SMB_BATTERY; Name: 'Portable Battery'),
    (Typ: SMB_RESET; Name: 'System Reset'),
    (Typ: SMB_SECURITY; Name: 'Hardware Security'),
    (Typ: SMB_POWER; Name: 'System Power Controls'),
    (Typ: SMB_VOLTAGE; Name: 'Voltage Probe'),
    (Typ: SMB_COOL; Name: 'Cooling Device'),
    (Typ: SMB_TEMP; Name: 'Temperature Probe'),
    (Typ: SMB_CURRENT; Name: 'Electrical Current Probe'),
    (Typ: SMB_OOBREM; Name: 'Out-of-Band Remote Access'),
    (Typ: SMB_BIS; Name: 'Boot Integrity Services (BIS) Entry Point'),
    (Typ: SMB_SYSBOOT; Name: 'System Boot Information'),
    (Typ: SMB_MEMERR64; Name: '64-bit Memory Error Information'),
    (Typ: SMB_MGT; Name: 'Management Device'),
    (Typ: SMB_MGTCMP; Name: 'Management Device Component'),
    (Typ: SMB_MGTTHR; Name: 'Management Device Threshold Data'),
    (Typ: SMB_MEMCHAN; Name: 'Memory Channel'),
    (Typ: SMB_IPMI; Name: 'IPMI Device Information'),
    (Typ: SMB_SPS; Name: 'System Power Supply'),
    (Typ: SMB_ADD; Name: 'Additional Information'),
    (Typ: SMB_ONBOARDX; Name: 'On Board Devices Extended Information'),
    (Typ: SMB_MGMTCTRL; Name: 'Management Controller Host Interface '),
    (Typ: SMB_TPMDEV; Name: 'TPM Device'),
    (Typ: SMB_INACTIVE; Name: 'Inactive'),
    (Typ: SMB_EOT; Name: 'End-of-Table'),
    (Typ: SMB_ACER_170; Name: 'Acer Hotkey Function'),
    (Typ: SMB_HP_204; Name: 'HP ProLiant System/Rack Locator'),
    (Typ: SMB_HP_209; Name: 'HP BIOS PXE NIC PCI and MAC Information'),
    (Typ: SMB_HP_221; Name: 'HP BIOS iSCSI NIC PCI and MAC Information'),
    (Typ: SMB_HP_233; Name: 'HP BIOS PXE NIC PCI and MAC Information'),
    (Typ: SMB_HP_212; Name: 'HP 64-bit CRU Information'),
    (Typ: SMB_HP_219; Name: 'HP ProLiant Information'),
    (Typ: SMB_IBM_131; Name: 'ThinkVantage Technologies feature bits'),
    (Typ: SMB_IBM_135; Name: 'Device Presence Detection bits'),
    (Typ: SMB_IBM_140; Name: 'ThinkPad Embedded Controller Program')
    );

type
  TSMBIOS_StructTable = record
    Address: Cardinal;
    Indicator: Byte;
    Length: Byte;
    Handle: Word;
    Name: string;
  end;

  TSMBIOS_StructTables = array of TSMBIOS_StructTable;

  TSMBIOS_BIOSChar = (smbcISA,smbcMCA,smbcEISA,smbcPCI,smbcPCMCIA,smbcPNP,smbcAPM,
                       smbcFlashBIOS,smbcBIOSShadow,smbcVLVESA,smbcESCD,smbcBootFromCD,
                       smbcSelectableBoot,smbcBIOSROMSocketed,smbcBootFromPCCard,
                       smbcEDD,smbcNECPC98,smbcACPI,smbcUSBLegacy,smbcAGP,smbcI2OBoot,
                       smbcLS120Boot,smbcATAPIZIPDriveBoot,smbcIEE1394Boot,
                       smbcSmartBattery,smbcBIOSBootSpec,smbcFKINSBoot,smbcTCD,
                       smbcUEFISpec,smbcVM);
  TSMBIOS_BIOSChars = set of TSMBIOS_BIOSChar;

  TSMBIOS_Chassis = (smchOther, smchUnknown, smchDesktop, smchLowProfileDesktop, smchPizzaBox,
              smchMiniTower, smchTower, smchPortable, smchLapTop, smchNotebook, smchHandHeld,
              smchDockingStation, smchAllInOne, smchSubNotebook, smchSpaceSaving, smchLunchBox,
              smchMainServer, smchExpansion, smchSubChassis, smchBusExpansion, smchPeripheral,
              smchRAID, smchRackMount, smchSealedCasePC, smchMultiSystem, smchCompactPCI,
              smchAdvTCA, smchBlade, smchBladeEnc);

  TSMBIOS_InterleaveSupport = (smisOther, smisUnknown, smisOnewWay, smisTwoWay, smisFourWay, smisEightWay, smisSixteenWay);

  TSMBIOS_Voltage = (smv5V, smv33V, smv29V);

  TSMBIOS_Voltages = set of TSMBIOS_Voltage;

  TSMBIOS_MemorySpeed = (smmsOther, smmsUnknown, smms70ns, smms60ns, smms50ns);

  TSMBIOS_MemorySpeeds = set of TSMBIOS_MemorySpeed;

  TSMBIOS_MemoryType = (smmtOther, smmtUnknown, smmtStandard, smmtFastPageMode, smmtEDO,
                 smmtParity, smmtECC, smmtSIMM, smmtDIMM, smmtBurstEDO, smmtSDRAM);

  TSMBIOS_MemoryTypes = set of TSMBIOS_MemoryType;

  TSMBIOS_MemoryFormFactor = (smffOther, smffUnknown, smffSIMM, smffSIP,
                       smffChip, smffDIP, smffZIP, smffPropCard, smffDIMM, smffTSOP,
                       smffRowChip, smffRIMM, smffSODIMM, smffSRIMM, smffFBDIMM);

  TSMBIOS_MemoryDeviceType = (smmdOther, smmdUnknown, smmdDRAM, smmdEDRAM, smmdVRAM, smmdSRAM,
                   smmdRAM,smmdROM, smmdFLASH, smmdEEPROM, smmdFEPROM, smmdEPROM,
                   smmdCDRAM, smmd3DRAM, smmdSDRAM, smmdSGRAM, smmdRDRAM, smmdDDR,
                   smmdDDR2, smmdDDR2FBDIMM, smmdReserved15h, smmdReserved16h, smmdReserved17h,
                   smmdDDR3, smmdFBD2, smmdDDR4, smmdLPDDR, smmdLPDDR2, smmdLPDDR3, smmdLPDDR4);

  TSMBIOS_MemoryTypeDetail = (mtdReserved, mtdOther, mtdUnknown, mtdFastPaged, mtdStaticColumn,
                       mtdPseudoStatic, mtdRAMBUS, mtdSynchronous, mtdCMOS, mtdEDO,
                       mtdWindowDRAM, mtdCacheDRAM, mtdNonVolatile, mtdRegistered, mtdUnbuffered,
                       mtdLRDIMM);

  TSMBIOS_MemoryTypeDetails = set of TSMBIOS_MemoryTypeDetail;

  TSMBIOS_MemoryTechnology = (mtOther, mtUnknown, mtDRAM, mtNVDIMMN, mtNVDIMMF, mtNVDIMMP, mtIntelPersistentMemory);

  TSMBIOS_Upgrade = (smuOther, smuUnknown, smuDaughterBoard, smuZIFSocket, smuReplaceablePiggyBack,
              smuNone, smuLIFSocket, smuSlot1, smuSlot2, smu370pinSocket, smuSlotA,
              smuSlotM, smuSocket423, smuSocketA, smuSocket478, smuSocket754, smuSocket940,
              smuSocket939, smuSocketmPGA604, smuSocketLGA771, smuSocketLGA775, smuSocketS1,
              smuSocketAM2, smuSocketF1207, smuSocketLGA1366, smuSocketG34, smuSocketAM3,
              smuSocketC32,smuSocketLGA1156,smuSocketLGA1567,smuSocketPGA988A,smuSocketBGA1288,
              smuSocketrPGA988B,smuSocketBGA1023,smuSocketBGA1224,smuSocketBGA1155,
              smuSocketLGA1356,smuSocketLGA2011,smuSocketFS1,smuSocketFS2,smuSocketFM1,
              smuSocketFM2,smuSocketLGA2011_3,smuSocketLGA1356_3);

  TSMBIOS_Processor = record
    Typ,
    Family: Byte;
    Socket,
    Manufacturer,
    Version: string;
    Upgrade: TSMBIOS_Upgrade;
    Voltage: double;
    Frequency,
    ExternalClock: WORD;
    SerialNumber,
    AssetTag,
    PartNumber: string;
    CoreCount,
    CoreEnabled,
    ThreadCount: Byte;
    Flags: Word;
    Family2: Word;
  end;

  TSMBIOS_MemoryModule = record
    Socket: string;
    Speed: Word;
    Size: Cardinal;
    Types: TSMBIOS_MemoryTypes;
  end;

  TSMBIOS_MemoryDevice = record
    TotalWidth,
    DataWidth,
    Size: Cardinal;
    FormFactor: TSMBIOS_MemoryFormFactor;
    DeviceLocator,
    BankLocator: string;
    Device: TSMBIOS_MemoryDeviceType;
    TypeDetails: TSMBIOS_MemoryTypeDetails;
    Speed,MaxSpeed: Word;
    Manufacturer,
    SerialNumber,
    AssetTag,
    PartNumber: string;
    MinimumVoltage,
    MaximumVoltage,
    ConfiguredVoltage: word;
    MemoryTechnology: TSMBIOS_MemoryTechnology;
    OperatingModeCapability: Word;
    FirmwareVersion: Byte;
    ModuleManufacturerID: Word;
    ModuleProductID: word;
    SubsystemControllerManufacturerID: Word;
    SubsystemControllerProductID: word;
    NonVolatileSize: int64;
    VolatileSize: int64;
    CacheSize: int64;
    LogicalSize: int64;
  end;

  TSMBIOS_ConnectorType = (smctNone, smctCentronics, smctMiniCentronics, smctProprietary,
                    smctDB25PinMale, smctDB25PinFemale, smctDB15PinMale, smctDB15PinFemale,
                    smctDB9PinMale, smctDB9PinFemale, smctRJ11, smctRJ45, smct50PinMiniSCSI,
                    smctMiniDIN, smctMicroDIN, smctPS2, smctInfrared, smctHPHIL,
                    smctAccessBus, smctSSASCSI, smctCircularDIN8Male, smctCircularDIN8Female,
                    smctOnBoardIDE, smctOnBoardFloppy, smct9PinDualInline, smct25PinDualInline,
                    smct50PinDualInline, smct68PinDualInline, smctOnBoardSoundInputFromCDROM,
                    smctMiniCentronicsType14, smctMiniCentronicsType26, smctMiniJack,
                    smctBNC, smct1394, smctSASSATA, smctPC98, smctPC98Hireso, smctPCH98, smctPC98Note,
                    smctPC98Full, smctOther);

  TSMBIOS_PortType = (smptNone, smptParallelXTAT, smptParallelPS2, smptParallelECP,
               smptParallelEPP, smptParallelECPEPP, smptSerialXTAT,
               smptSerial16450, smptSerial16550, smptSerial16550A,
               smptSCSI, smptMIDI, smptJoyStick, smptKeyboard, smptMouse, smptSSASCSI,
               smptUSB, smptFireWire, smptPCMCIA2, smptPCMCIA2A, smptPCMCIA3, smptCardbus,
               smptAccessBus, smptSCSI2, smptSCSIWide, smptPC98, smptPC98Hireso, smptPCH98,
               smptVideo, smptAudio, smptModem, smptNetwork, smptSATA, smptSAS,
               smpt8251, smpt8251FIFO, smptOther);

  TSMBIOS_Port = record
    InternalDesignator,
    ExternalDesignator: string;
    InternalConnector,
    ExternalConnector: TSMBIOS_ConnectorType;
    Typ: TSMBIOS_PortType;
  end;

  TSMBIOS_SlotType = (smstOther, smstUnknown, smstISA, smstMCA, smstEISA, smstPCI, smstPCMCIA,
               smstVLVESA, smstProprietary, smstProcessorCard, smstProprietaryMemoryCard,
               smstIORiserCard, smstNuBus, smstPCI66MHz, smstAGP, smstAGP2X, smstAGP4X,
               smstPCIX, smstAGP8X,
               smstPC98C20, smstPC98C24, smstPC98E, smstPC98LocalBus, smstPC98Card,
               smstPCIE, smstPCIEX1, smstPCIEX2, smstPCIEX4, smstPCIEX8, smstPCIEX16,
               smstPCIEG2, smstPCIEG2X1, smstPCIEG2X2, smstPCIEG2X4, smstPCIEG2X8, smstPCIEG2X16,
               smstPCIEG3,smstPCIEG3X1,smstPCIEG3X2,smstPCIEG3X4,smstPCIEG3X8,smstPCIEG3X16);

  TSMBIOS_DataBusType = (smdbOther, smdbUnknown, smdb8bit, smdb16bit, smdb32bit, smdb64bit, smdb128bit,
                         smdbX1, smdbX2, smdbX4, smdbX8, smdbX12, smdbX16, smdbX32);

  TSMBIOS_SlotUsage = (smsuOther, smsuUnknown, smsuAvailable, smsuInUse);

  TSMBIOS_SlotLength = (smslOther, smslUnknown, smslShort, smslLong);

  TSMBIOS_Slot = record
    Designation: string;
    Typ: TSMBIOS_SlotType;
    DataBus: TSMBIOS_DataBusType;
    ID: WORD;
    Usage: TSMBIOS_SlotUsage;
    Length: TSMBIOS_SlotLength;
    SegmentGroup: Word;
    BusNumber,
    DevNumber,
    FuncNumber: Byte;
  end;

  TSMBIOS_SRAMType = (sramOther, sramUnknown, sramNonBurst, sramBurst,
               sramPipelineBurst, sramSync, sramAsync);

  TSMBIOS_CacheType = (ctOther, ctUnknown, ctInstruction, ctData, ctUnified);

  TSMBIOS_CacheAssociativity = (caOther,caUnknown,caDirectMapped,ca2way,ca4way,
                                caFull,ca8way,ca16way,ca12Way,ca24Way,ca32Way,
                                ca48Way,ca64Way,ca20Way);

  TSMBIOS_Cache = record
    Designation: string;
    MaxSize, InstalledSize: Word;
    SRAMType: TSMBIOS_SRAMType;
    Typ: TSMBIOS_CacheType;
    Associativity: TSMBIOS_CacheAssociativity;
    Speed: Word;
  end;

  TSMBIOS_OnBoardDeviceType = (obdOther, obdUnknown, obdVideo, obdSCSICrl, obdEthernet, obdTokenRing, obdSound, obdPATA, obdSATA, obdSAS);

  TSMBIOS_OnBoardDevice = record
    DeviceName: string;
    Typ: TSMBIOS_OnBoardDeviceType;
    Status: boolean;
  end;

  TSMBIOS_StatusType = (stOther, stUnknown, stOK, stNonCritical, stCritical, stNonRecoverable);

  TSMBIOS_VoltProbeLocationType = (vpOther, vpUnknown, vpProcessor, vpDisk, vpPeripheralBay, vpSMM, vpMB,
                   vpMemoryModule, vpProcessorModule, vpPowerUnit, vpAddInCard);

  TSMBIOS_VoltageProbe = record
    Description: string;
    Location: TSMBIOS_VoltProbeLocationType;
    Status: TSMBIOS_StatusType;
    Min,Max: Word;
    Resolution,
    Tolerance,
    Accuracy: Word;
    NominalValue: Word;
  end;

  TSMBIOS_TempProbeLocationType = (tpOther, tpUnknown, tpProcessor, tpDisk, tpPeripheralBay, tpSMM, tpMB,
                   tpProcessorModule, tpPowerUnit, tpAddInCard, tpFrontPanelBoard, tpBackPanelBoard,
                   tpPowerSystemBoard, tpDriveBackPlane);

  TSMBIOS_TemperatureProbe = record
    Description: string;
    Location: TSMBIOS_TempProbeLocationType;
    Status: TSMBIOS_StatusType;
    Min,Max: Word;
    Resolution,
    Tolerance,
    Accuracy: Word;
    NominalValue: Word;
  end;

  TSMBIOS_CurrProbeLocationType = (cpOther, cpUnknown, cpProcessor, cpDisk, cpPeripheralBay, cpSMM, cpMB,
                   cpMemoryModule, cpProcessorModule, cpPowerUnit, cpAddInCard);

  TSMBIOS_CurrentProbe = record
    Description: string;
    Location: TSMBIOS_CurrProbeLocationType;
    Status: TSMBIOS_StatusType;
    Min,Max: Word;
    Resolution,
    Tolerance,
    Accuracy: Word;
    NominalValue: Word;
  end;

  TSMBIOS_CoolingType = (cdOther, cdUnknown, cdFan, cdCentrifugalBlower, cdChipFan, cdCabinetFan,
                         cdPowerSupplyFan, cdHeatPipe, cdIntegratedRefrigeration, cdActiveCooling,
                         cdPassiveCooling);

  TSMBIOS_CoolingDevice = record
    TemperatureProbeHandle: Word;
    Description: string;
    Status: TSMBIOS_StatusType;
    Typ: TSMBIOS_CoolingType;
    NominalSpeed: Cardinal;
    GroupUnit: Byte;
  end;

  TSMBIOS_PMALocation = (pmalOther, pmalUnknown, pmalSystemBoard, pmalISA, pmalEISA, pmalPCI,
                         pmalMCA, pmalPCMCIA, pmalProprietary, pmalNuBus, pmalPC98C20, pmalPC98C24,
                         pmalPC98E, pmalPC98LocalBus);

  TSMBIOS_PMAUse = (pmauOther, pmauUnknown, pmauSystemMemory, pmauVideoMemory, pmauFlashMemory, pmauNonVolatileRAM, pmauCacheMemory);

  TSMBIOS_PMAErrorCorrectionType = (pmaectOther, pmaectUnknown, pmaectNone, pmaectParity, pmaectSinglebitECC, pmaectMultibitECC, pmaectCRC);

  TSMBIOS_OnBoardDeviceEx = record
    DeviceName: string;
    Typ: TSMBIOS_OnBoardDeviceType;
    Instance: Byte;
    SegmentGroupNumber: Word;
    BusNumber: Byte;
    DeviceNumber: Byte;
    FunctionNumber: Byte;
    Status: Boolean;
  end;

  TSMBIOS_TPMDevice = record
    VendorID: string;
    MajorSpecVersion: byte;
    MinorSpecVersion: Byte;
    FirmwareVersion1: Cardinal;
    FirmwareVersion2: Cardinal;
    Description: string;
    Chars: int64;
    OEMDef: Cardinal;
  end;

  TSMBIOS_DeviceChemistry = (dcOther, dcUnknown, dcLeadAcid, dcNickelCadmium, dcNickelMetalHydride, dcLithiumIon, dcZincAir, dcLithiumPolymer);

  TSMBIOS_Battery = record
    Location: string;
    Manufacturer: string;
    ManufacturerDate: string;
    SerialNumber: string;
    DeviceName: string;
    DeviceChemistry: TSMBIOS_DeviceChemistry;
    DesignCapacity: Word;    //mWh
    DesignVoltage: Word;     // mV
    SBDSVersionNumber: string;
    MaxErrorInBatData: Word;
    SBDSSerialNumber: word;
    SBDSManufactureDate: word;
    SBDSDeviceChemistry: string;
    DesignCapacityMultiplier: byte;
    OEMSpec: Cardinal;
  end;

  TSMBIOS_HP_204 = record
    RackName: string;
    EnclosureName: string;
    EnclosureModel: string;
    ServerBay: string;
    EnclosureBays: Byte;
    BaysFilled: byte;
    EnclosureSerial: string;
  end;

  TSMBIOS_HP_221 = record
    DevNo: Byte;
    BusNo: byte;
    MAC: string; //6B
  end;

  TSMBIOS_HP_233 = record
    GrpNo: word;
    BusNo: byte;
    DevNo: byte;
    MAC: string; //32B, 0-padded
    PortNo: byte;
  end;

  TSMBIOS_HP_219 = record
    PowerFeatures: Cardinal;
    OmegaFeatures: Cardinal;
    MiscFeatures: Cardinal;
  end;

const
  BIOSChars: array[TSMBIOS_BIOSChar] of string =
    ('ISA','MCA','EISA','PCI','PCMCIA','Plug-and-Play','APM','Flash BIOS',
     'BIOS Shadow','VL-VESA','ESCD','Boot from CD','Selectable Boot','BIOS ROM Socketed',
     'Boot from PC Card','EDD','NEC PC-98','ACPI','USB Legacy','AGP','I2O Boot',
     'LS-120 Boot','ATAPI ZIP Drive Boot','IEE1394 Boot','Smart Battery','BIOS Boot Specification',
     'Function key-initiated Network Service Boot','Targeted Content Distribution',
     'UEFI Specification','Virtual Machine');

  ChassisTypes: array[TSMBIOS_Chassis] of string = ('Other','Unknown','Desktop','Low Profile Desktop','Pizza Box',
              'Mini Tower','Tower','Portable','LapTop','Notebook','Hand Held',
              'Docking Station','All in One','SubNotebook','Space-Saving','Lunch Box',
              'Main Server Chassis','Expansion Chassis','SubChassis','Bus Expansion Chassis','Peripheral Chassis',
              'RAID Chassis','Rack-Mount Chassis','Sealed-case PC','Multi-system Chassis',
              'CompactPCI','AdvancedTCA','Blade','Blade Enclosure');

  InterleaveSupports: array[TSMBIOS_InterleaveSupport] of string = ('Other','Unknown','1-Way','2-Way','4-Way','8-Way','16-Way');

  Voltages: array[TSMBIOS_Voltage] of string = ('5V','3.3V','2.9V');

  MemorySpeeds: array[TSMBIOS_MemorySpeed] of string = ('Other','Unknown','70ns','60ns','50ns');

  Upgrades: array[TSMBIOS_Upgrade] of string = ('Other','Unknown','Daughter Board','ZIF Socket','Replaceable Piggy Back',
              'None','LIF Socket','Slot 1','Slot 2','370-pin Socket','Slot A',
              'Slot M','Socket 423', 'Socket A (Socket 462)','Socket 478','Socket 754','Socket 940',
              'Socket 939', 'Socket mPGA604', 'Socket LGA771', 'Socket LGA775', 'Socket S1',
              'Socket AM2', 'Socket F (1207)', 'Socket LGA1366','Socket G34','Socket AM3',
              'Socket C32','Socket LGA1156','SocketLGA1567','Socket PGA988A','Socket BGA1288',
              'Socket rPGA988B','Socket BGA1023','Socket BGA1224','Socket BGA1155',
              'Socket LGA1356','Socket LGA2011','Socket FS1','Socket FS2','Socket FM1',
              'Socket FM2','Socket LGA2011-3','Socket LGA1356-3');

  MemoryFormFactors: array[TSMBIOS_MemoryFormFactor] of string = ('Other','Unknown','SIMM','SIP',
    'Chip','DIP','ZIP','PropCard','DIMM','TSOP','RowChip','RIMM','SODIMM','SRIMM','FB-DIMM');

  MemoryDeviceTypes: array[TSMBIOS_MemoryDeviceType] of string = ('Other','Unknown','DRAM','EDRAM',
    'VRAM','SRAM','RAM','ROM','FLASH','EEPROM','FEPROM','EPROM','CDRAM','3DRAM','SDRAM',
    'SGRAM','RDRAM','DDR','DDR2','DDR 2FB-DIMM','Reserved15h','Reserved16h','Reserved17h','DDR3','FBD2',
    'DDR4','LPDDR','LPDDR2','LPDDR3','LPDDR4');

  MemoryTypes: array[TSMBIOS_MemoryType] of string = ('Other','Unknown','Standard','Fast Page Mode','EDO',
                 'Parity','ECC','SIMM','DIMM','Burst EDO','SDRAM');

  MemoryTypeDetails: array[TSMBIOS_MemoryTypeDetail] of string = ('Reserved', 'Other', 'Unknown', 'FastPaged',
                               'StaticColumn', 'PseudoStatic', 'RAMBUS', 'Synchronous', 'CMOS', 'EDO',
                               'WindowDRAM', 'CacheDRAM', 'NonVolatile', 'Registered', 'Unbuffered', 'LRDIMM');

 MemoryTechnologies: array[TSMBIOS_MemoryTechnology] of string =  ('Other', 'Unknown', 'DRAM', 'NVDIMM-N', 'NVDIMM-F', 'NVDIMM-P', 'Intel Persistent Memory');

  ConnectorTypes: array[TSMBIOS_ConnectorType] of string = (
    'None','Centronics','Mini Centronics','Proprietary','DB-25 pin male','DB-25 pin female',
    'DB-15 pin male','DB-15 pin female','DB-9 pin male','DB-9 pin female','RJ-11','RJ-45',
    '50 Pin MiniSCSI','Mini-DIN','Micro-DIN','PS/2','Infrared','HP-HIL','Access Bus (USB)',
    'SSA SCSI','Circular DIN-8 male','Circular DIN-8 female','On Board IDE','On Board Floppy',
    '9 Pin Dual Inline (pin 10 cut)','25 Pin Dual Inline (pin 26 cut)','50 Pin Dual Inline',
    '68 Pin Dual Inline','On Board Sound Input from CD-ROM','Mini-Centronics Type-14',
    'Mini-Centronics Type-26','Mini-jack (headphones)','BNC','1394','SAS/SATA Plug Receptacle','PC-98','PC-98Hireso',
    'PC-H98','PC-98Note','PC-98Full','Other'
  );

  PortTypes: array[TSMBIOS_PortType] of string = (
    'None','Parallel Port XT/AT Compatible','Parallel Port PS/2','Parallel Port ECP',
    'Parallel Port EPP','Parallel Port ECP/EPP','Serial Port XT/AT Compatible',
    'Serial Port 16450 Compatible','Serial Port 16550 Compatible','Serial Port 16550A Compatible',
    'SCSI Port','MIDI Port','Joy Stick Port','Keyboard Port','Mouse Port','SSA SCSI',
    'USB','FireWire (IEEE P1394)','PCMCIA Type II','PCMCIA Type II','PCMCIA Type III',
    'Cardbus','Access Bus Port','SCSI II','SCSI Wide','PC-98','PC-98-Hireso','PC-H98',
    'Video Port','Audio Port','Modem Port','Network Port','SATA','SAS','8251 Compatible',
    '8251 FIFO Compatible','Other'
  );

  SlotTypes: array[TSMBIOS_SlotType] of string = (
    'Other','Unknown','ISA','MCA','EISA','PCI','PC Card (PCMCIA)','VL-VESA','Proprietary',
    'Processor Card Slot','Proprietary Memory Card','I/O Riser Card Slot','NuBus',
    'PCI - 66MHz Capable','AGP','AGP 2X','AGP 4X','PCI-X','AGP 8X', 'PC-98/C20','PC-98/C24','PC-98/E',
    'PC-98/Local Bus','PC-98/Card','PCI-Express','PCI-Express x1','PCI-Express x2','PCI-Express x4',
    'PCI-Express x8','PCI-Express x16','PCI-Express Gen 2','PCI-Express Gen 2 x1','PCI-Express Gen 2 x2',
    'PCI-Express Gen 2 x4','PCI-Express Gen 2 x8','PCI-Express Gen 2 x16',
    'PCI-Express Gen 3','PCI-Express Gen 3 x1','PCI-Express Gen 3 x2','PCI-Express Gen 3 x4',
    'PCI-Express Gen 3 x8','PCI-Express Gen 3 x16'
  );

  DataBusTypes: array[TSMBIOS_DataBusType] of string = (
    'Other','Unknown','8 bit','16 bit','32 bit','64 bit','128 bit',
    '1x or x1','2x or x2','4x or x4','8x or x8','12x or x12','16x or x16','32x or x32'
  );

  SlotUsages: array[TSMBIOS_SlotUsage] of string = ('Other','Unknown','Available','InUse');

  SlotLengths: array[TSMBIOS_SlotLength] of string = ('Other','Unknown','Short','Long');

  SRAMTypes: array[TSMBIOS_SRAMType] of string = (
    'Other', 'Unknown','Non-Burst','Burst','Pipeline Burst','Synchronous','Asynchronous'
  );

  CacheTypes: array[TSMBIOS_CacheType] of string = (
    'Other','Unknown','Instruction','Data','Unified'
  );

  CacheAssociativities: array[TSMBIOS_CacheAssociativity] of string = (
    'Other','Unknown','Direct Mapped','2-way Set-Associative','4-way Set-Associative','Fully Associative',
    '8-way Set-Associative','16-way Set-Associative','12-way Set-Associative','24-way Set-Associative',
    '32-way Set-Associative','48-way Set-Associative','64-way Set-Associative','20-way Set-Associative'
  );

  OnBoardDeviceTypes: array[TSMBIOS_OnBoardDeviceType] of string = (
    'Other', 'Unknown', 'Video', 'SCSI Controller', 'Ethernet', 'TokenRing', 'Sound', 'PATA Controller', 'SATA Controller', 'SAS Controller');

  TempProbeLocationTypes: array[TSMBIOS_TempProbeLocationType] of string = ('Other', 'Unknown', 'Processor', 'Disk', 'Peripheral Bay',
    'System Management Module', 'Motherboard', 'Processor Module', 'Power Unit', 'Add-In Card', 'Front Panel Board',
    'Back Panel Board', 'Power System Board', 'Drive Back Plane');

  StatusTypes: array[TSMBIOS_StatusType] of string = ('Other', 'Unknown', 'OK', 'Non-Critical', 'Critical', 'Non-Recoverable');

  CoolingTypes: array[TSMBIOS_CoolingType] of string = ('Other', 'Unknown', 'Fan', 'Centrifugal Blower', 'Chip Fan',
                         'Cabinet Fan', 'Power Supply Fan', 'Heat Pipe', 'Integrated Refrigeration', 'Active Cooling',
                         'Passive Cooling');

  VoltProbeLocationTypes: array[TSMBIOS_VoltProbeLocationType] of string = ('Other', 'Unknown', 'Processor',
                   'Disk', 'Peripheral Bay', 'System Management Module', 'Motherboard',
                   'Memory Module', 'Processor Module', 'Power unit', 'Add-In Card');

  CurrProbeLocationTypes: array[TSMBIOS_CurrProbeLocationType] of string = ('Other', 'Unknown', 'Processor',
                   'Disk', 'Peripheral Bay', 'System Management Module', 'Motherboard',
                   'Memory Module', 'Processor Module', 'Power unit', 'Add-In Card');

  PMALocations: array[TSMBIOS_PMALocation] of string = ('Other', 'Unknown' ,'System board or motherboard', 'ISA add-on card',
                                                 'EISA add-on card', 'PCI add-on card', 'MCA add-on card', 'PCMCIA add-on card',
                                                 'Proprietary add-on card', 'NuBus', 'PC-98/C20 add-on card', 'PC-98/C24 add-on card',
                                                 'PC-98/E add-on card', 'PC-98/Local bus add-on card');
  PMAUses: array[TSMBIOS_PMAUse] of string = ('Other', 'Unknown', 'System memory', 'Video memory', 'Flash memory', 'Non-volatile RAM', 'Cache memory');
  PMAErrorCorrectionTypes: array[TSMBIOS_PMAErrorCorrectionType] of string = ('Other', 'Unknown', 'None', 'Parity', 'Single-bit ECC',
                                                                              'Multi-bit ECC', 'CRC');

  DeviceChemistries: array[TSMBIOS_DeviceChemistry] of string = ('Other', 'Unknown', 'Lead Acid', 'Nickel Cadmium', 'Nickel metal hydride', 'Lithium-Ion', 'Zinc Air', 'Lithium Polymer');

type
  TMiTeC_SMBIOS = class(TMiTeC_Component)
  private
    FSMBIOS, FStructure: TMiTeC_DMA;
    FStart,
    FStructStart: Cardinal;
    FMBMod: string;
    FSysMan: string;
    FSysMod: string;
    FBIOSdate: string;
    FBIOSVendor: string;
    FBIOSVersion: string;
    FBIOSSize: Cardinal;
    FBIOSMajor,
    FBIOSMinor,
    FBIOSECFMajor,
    FBIOSECFMinor: Byte;
    FSysVer: string;
    FSysSN: string;
    FMBSN: string;
    FMBMan: string;
    FMBVer: string;
    FCHSN: string;
    FCHVer: string;
    FCHMan: string;
    FCHMod: TSMBIOS_Chassis;
    FSysUUID: string;
    FProc: array of TSMBIOS_Processor;
    FMemMod: array of TSMBIOS_MemoryModule;
    FMemDev: array of TSMBIOS_MemoryDevice;
    FPort: array of TSMBIOS_Port;
    FSlot: array of TSMBIOS_Slot;
    FCache: array of TSMBIOS_Cache;
    FOBD: array of TSMBIOS_OnBoardDevice;
    FOBDX: array of TSMBIOS_OnBoardDeviceEx;
    FTP: array of TSMBIOS_TemperatureProbe;
    FCD: array of TSMBIOS_CoolingDevice;
    FCP: array of TSMBIOS_CurrentProbe;
    FVP: array of TSMBIOS_VoltageProbe;
    FTPM: array of TSMBIOS_TPMDevice;
    FBat: array of TSMBIOS_Battery;
    FTableCount: WORD;
    FLen: WORD;
    FMCSC: Byte;
    FMCCI: TSMBIOS_InterleaveSupport;
    FMCSI: TSMBIOS_InterleaveSupport;
    FMCSS: TSMBIOS_MemorySpeeds;
    FMCST: TSMBIOS_MemoryTypes;
    FMCSV: TSMBIOS_Voltages;
    FMCMS: WORD;
    FCHAT: string;
    FMajorVersion: Byte;
    FMinorVersion: Byte;
    FMBAT: string;
    FMBLIC: string;
    FLocal: boolean;
    FBIOSChars: TSMBIOS_BIOSChars;
    FUEFI: boolean;
    FNoWMI: Boolean;
    FPMALoc: TSMBIOS_PMALocation;
    FPMAUse: TSMBIOS_PMAUse;
    FPMAECT: TSMBIOS_PMAErrorCorrectionType;
    FPMAMC: Int64;
    FPMADN: word;
    FSPSMPN: string;
    FSPSIVPH: Word;
    FSPSPUG: Byte;
    FSPSSN: string;
    FSPSLoc: string;
    FSPSCDH: Word;
    FSPSC: Word;
    FSPSRL: string;
    FSPSM: string;
    FSPSICPH: Word;
    FSPSATN: string;
    FSPSMPC: Word;
    FSPSDN: string;
    FHP204: TSMBIOS_HP_204;
    FHP209, FHP221: array of TSMBIOS_HP_221;
    FRevisionMinor: Byte;
    FRevisionMajor: Byte;
    procedure ScanTables;
    procedure AddTable(Addr: Cardinal; Ind,Len: Byte; Hndl: Word; var ST: TSMBIOS_StructTables);
    function GetMemoryModule(Index: Byte): TSMBIOS_MemoryModule;
    function GetMemModCount: Byte;
    function GetPort(Index: Byte): TSMBIOS_Port;
    function GetPortCount: Byte;
    function GetSlot(Index: Byte): TSMBIOS_Slot;
    function GetSlotCount: Byte;
    function GetCache(Index: Byte): TSMBIOS_Cache;
    function GetCacheCount: Byte;
    function GetProc(Index: Byte): TSMBIOS_Processor;
    function GetProcCount: Byte;
    function GetOBDCount: Byte;
    function GetOBD(Index: Byte): TSMBIOS_OnBoardDevice;
    function GetTempProbe(Index: Byte): TSMBIOS_TemperatureProbe;
    function GetTempProbeCount: Byte;
    function GetMemoryDevice(Index: Byte): TSMBIOS_MemoryDevice;
    function GetMemDevCount: Byte;
    procedure SetProc(Index: Byte; const Value: TSMBIOS_Processor);
    procedure SetMemoryModule(Index: Byte; const Value: TSMBIOS_MemoryModule);
    procedure SetMemoryDevice(Index: Byte; const Value: TSMBIOS_MemoryDevice);
    procedure SetPort(Index: Byte; const Value: TSMBIOS_Port);
    procedure SetCache(Index: Byte; const Value: TSMBIOS_Cache);
    procedure SetOBD(Index: Byte; const Value: TSMBIOS_OnBoardDevice);
    procedure SetTempProbe(Index: Byte; const Value: TSMBIOS_TemperatureProbe);
    procedure SetSlot(Index: Byte; const Value: TSMBIOS_Slot);
    function GetCoolDev(Index: Byte): TSMBIOS_CoolingDevice;
    procedure SetCoolDev(Index: Byte; const Value: TSMBIOS_CoolingDevice);
    function GetCoolDevCount: Byte;
    function GetCurrProbe(Index: Byte): TSMBIOS_CurrentProbe;
    function GetCurrProbeCount: Byte;
    function GetVoltProbe(Index: Byte): TSMBIOS_VoltageProbe;
    function GetVoltProbeCount: Byte;
    procedure SetCurrProbe(Index: Byte; const Value: TSMBIOS_CurrentProbe);
    procedure SetVoltProbe(Index: Byte; const Value: TSMBIOS_VoltageProbe);
    function GetOBDX(Index: Byte): TSMBIOS_OnBoardDeviceEx;
    procedure SetOBDX(Index: Byte; const Value: TSMBIOS_OnBoardDeviceEx);
    function GetOBDXCount: Byte;
    function GetTPMD(Index: Byte): TSMBIOS_TPMDevice;
    function GetTPMDCount: Byte;
    procedure SetTPMD(Index: Byte; const Value: TSMBIOS_TPMDevice);
    function GetBat(Index: Byte): TSMBIOS_Battery;
    function GetBatCount: Byte;
    procedure SetBat(Index: Byte; const Value: TSMBIOS_Battery);
    function GetHP209(Index: Byte): TSMBIOS_HP_221;
    function GetHP209Count: Byte;
    function GetHP221(Index: Byte): TSMBIOS_HP_221;
    function GetHP221Count: Byte;
    procedure SetHP209(Index: Byte; const Value: TSMBIOS_HP_221);
    procedure SetHP221(Index: Byte; const Value: TSMBIOS_HP_221);
  public
    StructTables: TSMBIOS_StructTables;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;

    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;

    function FindTableRecord(AType: Byte; ST: TSMBIOS_StructTables; From: Cardinal = 0): TSMBIOS_StructTable;
    function FindTableIndex(AType: Byte; ST: TSMBIOS_StructTables; From: Cardinal = 0): Integer;

    procedure LoadSMBIOSFromFile(const AFilename: string);
    procedure LoadRawMemoryFromFile(const AFilename: string);

    property RAW_DMA: TMiTeC_DMA read FSMBIOS;
    property SMBIOS_DMA: TMiTeC_DMA read FStructure;

    property MajorVersion :Byte read FMajorVersion write FMajorVersion;
    property MinorVersion :Byte read FMinorVersion write FMinorVersion;
    property RevisionMajor: Byte read FRevisionMajor write FRevisionMajor;
    property RevisionMinor: Byte read FRevisionMinor write FRevisionMinor;
    property SMBIOSAddress: Cardinal read FStart;
    property StructStart: Cardinal read FStructStart;
    property StructLength: WORD read FLen;
    property StructCount: WORD read FTableCount;

    property ProcessorCount: Byte read GetProcCount;
    property Processor[Index: Byte]: TSMBIOS_Processor read GetProc write SetProc;
    property MemoryModuleCount: Byte read GetMemModCount;
    property MemoryModule[Index: Byte]: TSMBIOS_MemoryModule read GetMemoryModule write SetMemoryModule;
    property MemoryDeviceCount: Byte read GetMemDevCount;
    property MemoryDevice[Index: Byte]: TSMBIOS_MemoryDevice read GetMemoryDevice write SetMemoryDevice;
    property PortCount: Byte read GetPortCount;
    property Port[Index: Byte]: TSMBIOS_Port read GetPort write SetPort;
    property SystemSlotCount: Byte read GetSlotCount;
    property SystemSlot[Index: Byte]: TSMBIOS_Slot read GetSlot write SetSlot;
    property CacheCount: Byte read GetCacheCount;
    property Cache[Index: Byte]: TSMBIOS_Cache read GetCache write SetCache;
    property OnBoardDeviceCount: Byte read GetOBDCount;
    property OnBoardDevice[Index: Byte]: TSMBIOS_OnBoardDevice read GetOBD write SetOBD;
    property TemperatureProbeCount: Byte read GetTempProbeCount;
    property TemperatureProbe[Index: Byte]: TSMBIOS_TemperatureProbe read GetTempProbe write SetTempProbe;
    property CoolingDeviceCount: Byte read GetCoolDevCount;
    property CoolingDevice[Index: Byte]: TSMBIOS_CoolingDevice read GetCoolDev write SetCoolDev;
    property VoltageProbeCount: Byte read GetVoltProbeCount;
    property VoltageProbe[Index: Byte]: TSMBIOS_VoltageProbe read GetVoltProbe write SetVoltProbe;
    property CurrentProbeCount: Byte read GetCurrProbeCount;
    property CurrentProbe[Index: Byte]: TSMBIOS_CurrentProbe read GetCurrProbe write SetCurrProbe;
    property OnBoardDeviceExCount: Byte read GetOBDXCount;
    property OnBoardDeviceEx[Index: Byte]: TSMBIOS_OnBoardDeviceEx read GetOBDX write SetOBDX;
    property TPMDeviceCount: Byte read GetTPMDCount;
    property TPMDevice[Index: Byte]: TSMBIOS_TPMDevice read GetTPMD write SetTPMD;
    property BatteryCount: Byte read GetBatCount;
    property Battery[Index: Byte]: TSMBIOS_Battery read GetBat write SetBat;

    property HPProLiantSystemRackLocator: TSMBIOS_HP_204 read FHP204;
    property HPBIOSPXENICPCIMACInfoCount: Byte read GetHP209Count;
    property HPBIOSPXENICPCIMACInfo[Index: Byte]: TSMBIOS_HP_221 read GetHP209 write SetHP209;
    property HPBIOSiSCINICPCIMACInfoCount: Byte read GetHP221Count;
    property HPBIOSiSCINICPCIMACInfo[Index: Byte]: TSMBIOS_HP_221 read GetHP221 write SetHP221;
  published
    property ReadLocalMemory: boolean read FLocal write FLocal;
    property SystemModel: string read FSysMod stored False;
    property SystemManufacturer: string read FSysMan stored False;
    property SystemVersion: string read FSysVer stored False;
    property SystemSerial: string read FSysSN stored False;
    property SystemUUID: string read FSysUUID stored False;

    property BIOSVendor: string read FBIOSVendor stored False;
    property BIOSVersion: string read FBIOSVersion stored False;
    property BIOSDate: string read FBIOSdate stored False;
    property BIOSSize: Cardinal read FBIOSSize stored False;
    property BIOSMajorVersion: Byte read FBIOSMajor stored False;
    property BIOSMinorVersion: Byte read FBIOSMinor stored False;
    property BIOS_ECF_MajorVersion: Byte read FBIOSECFMajor stored False;
    property BIOS_ECF_MinorVersion: Byte read FBIOSECFMinor stored False;
    property BIOSCharacteristics: TSMBIOS_BIOSChars read FBIOSChars stored False;
    property UEFI: boolean read FUEFI stored False;


    property MainBoardModel: string read FMBMod stored False;
    property MainBoardManufacturer: string read FMBMan stored False;
    property MainBoardVersion: string read FMBVer stored False;
    property MainBoardSerial: string read FMBSN stored False;
    property MainBoardAssetTag: string read FMBAT stored False;
    property MainBoardLocationInChassis: string read FMBLIC stored False;

    property ChassisModel: TSMBIOS_Chassis read FCHMod stored False;
    property ChassisManufacturer: string read FCHMan stored False;
    property ChassisVersion: string read FCHVer stored False;
    property ChassisSerial: string read FCHSN stored False;
    property ChassisAssetTag: string read FCHAT stored False;

    property MemCtrlCurrentInterleave: TSMBIOS_InterleaveSupport read FMCCI stored False;
    property MemCtrlSupportedInterleave: TSMBIOS_InterleaveSupport read FMCSI stored False;
    property MemCtrlSupportedSpeeds: TSMBIOS_MemorySpeeds read FMCSS stored False;
    property MemCtrlSupportedTypes: TSMBIOS_MemoryTypes read FMCST stored False;
    property MemCtrlSupportedVoltages: TSMBIOS_Voltages read FMCSV stored False;
    property MemCtrlMaxSize: WORD read FMCMS stored False;
    property MemCtrlSlotCount: Byte read FMCSC stored False;

    property PMALocation: TSMBIOS_PMALocation read FPMALoc stored false;
    property PMAUse: TSMBIOS_PMAUse read FPMAUse stored false;
    property PMAErrorCorrectionType: TSMBIOS_PMAErrorCorrectionType read FPMAECT stored false;
    property PMAMaximumCapacity: Int64 read FPMAMC stored false;
    property PMANumberOfMemoryDevices: word read FPMADN stored false;

    property SystemPowerSupplyPowerUnitGroup: Byte read FSPSPUG stored false;
    property SystemPowerSupplyLocation: string read FSPSLoc stored false;
    property SystemPowerSupplyDeviceName: string read FSPSDN stored false;
    property SystemPowerSupplyManufacturer: string read FSPSM stored false;
    property SystemPowerSupplySerialNumber: string read FSPSSN stored false;
    property SystemPowerSupplyAssetTagNumber: string read FSPSATN stored false;
    property SystemPowerSupplyModelPartNumber: string read FSPSMPN stored false;
    property SystemPowerSupplyRevisionLevel: string read FSPSRL stored false;
    property SystemPowerSupplyMaxPowerCapacity: Word read FSPSMPC stored false;
    property SystemPowerSupplyChars: Word read FSPSC stored false;
    property SystemPowerSupplyInputVoltageProbeHandle: Word read FSPSIVPH stored false;
    property SystemPowerSupplyCoolingDeviceHandle: Word read FSPSCDH stored false;
    property SystemPowerSupplyInputCurrenyProbeHandle: Word read FSPSICPH stored false;

    property DisableWMI: Boolean read FNoWMI write FNoWMI;
  end;

  function GetBIOSCharStr(Value: TSMBIOS_BIOSChars): string;
  function GetMemoryTypeStr(Value: TSMBIOS_MemoryTypes): string;
  function GetMemorySpeedStr(Value: TSMBIOS_MemorySpeeds): string;
  function GetMemoryVoltageStr(Value: TSMBIOS_Voltages): string;
  function GetMemoryTypeDetailsStr(Value: TSMBIOS_MemoryTypeDetails): string;

implementation

uses {$IFDEF RAD9PLUS}
     System.Math,
     {$ELSE}
     Math,
     {$ENDIF}
     MiTeC_Routines, MiTeC_NativeAPI, MiTeC_NativeDefs, MiTeC_WMI, MiTeC_StrUtils,
     {$IFDEF FPC}MiTeC_FPC_WbemScripting_TLB{$ELSE}MiTeC_WbemScripting_TLB{$ENDIF};

{ TMiTeC_SMBIOS }

{ based on code by Nico Bendlin - BEGIN}
type
  TFNEnumSystemFirmwareTables = function(FirmwareTableProviderSignature: DWORD;
    out pFirmwareTableEnumBuffer; BufferSize: DWORD): UINT; stdcall;
  TFNGetSystemFirmwareTable = function(FirmwareTableProviderSignature: DWORD;
    FirmwareTableID: DWORD; out pFirmwareTableBuffer; BufferSize: DWORD): UINT;
    stdcall;
  TGetFirmwareEnvironmentVariable = function (lpName: PAnsiChar; lpGuid: PAnsiChar; pBuffer: Pointer;
                                              nSize: LongWord): LongWord; stdcall;
var
  EnumSystemFirmwareTables: TFNEnumSystemFirmwareTables = nil;
  GetSystemFirmwareTable: TFNGetSystemFirmwareTable = nil;
  GetFirmwareEnvironmentVariable: TGetFirmwareEnvironmentVariable = nil;

function DumpRomBiosFw(RomBase: Pointer; RomSize: Cardinal; out Dump): Boolean;
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..High(Integer) div SizeOf(Byte) - 1] of Byte;
  PDWordArray = ^TDWordArray;
  TDWordArray = array [0..High(Integer) div SizeOf(DWORD) - 1] of DWORD;
const
  FwTP = $4649524D;  // 'FIRM' The raw firmware table provider.
var
  HMod: HMODULE;
  Size: UINT;
  List: PDWordArray;
  i: Integer;
  BLen: UINT;
  Buff: PByteArray;
  Base: Boolean;
  Over: Boolean;
  BOff: UINT;
  DOff: UINT;
begin
  List:=nil;
  Result:=False;
  HMod:=GetModuleHandle(kernel32);
  if HMod=0 then
    SetLastError(ERROR_CALL_NOT_IMPLEMENTED)
  else begin
    if not Assigned(EnumSystemFirmwareTables) then
      EnumSystemFirmwareTables:=GetProcAddress(HMod, 'EnumSystemFirmwareTables');
    if not Assigned(GetSystemFirmwareTable) then
      GetSystemFirmwareTable:=GetProcAddress(HMod, 'GetSystemFirmwareTable');
    if not Assigned(EnumSystemFirmwareTables) or not Assigned(GetSystemFirmwareTable) then
      Exit;

    Size:=EnumSystemFirmwareTables(FwTP,nil^,0)+2*SizeOf(DWORD);
    if Size>0 then
      List:=PDWordArray(LocalAlloc(LPTR,Size));
    if List<>nil then
      try
        Base:=False;
        Over:=False;
        ResetMemory(Dump,RomSize);
        Size:=EnumSystemFirmwareTables(FwTP, List^, Size);
        for i:=0 to Integer(Size div SizeOf(DWORD))-1 do
          if List[i]<Cardinal(RomBase)+RomSize then begin
            BLen:=GetSystemFirmwareTable(FwTP,List[i],nil^,0)+$20000;
            Buff:=PByteArray(LocalAlloc(LPTR,BLen));
            if Buff<>nil then
              try
                BLen:=GetSystemFirmwareTable(FwTP,List[i],Buff^,BLen);
                if (BLen>0) and (List[i]+BLen>Cardinal(RomBase)) then begin
                  if List[i]<=Cardinal(RomBase) then begin
                    Base:=True;
                    BOff:=Cardinal(RomBase)-List[i];
                    DOff:=0;
                  end else begin
                    BOff:=0;
                    DOff:=List[i]-Cardinal(RomBase);
                  end;
                  if DOff+BLen>=RomSize then  begin
                    Over:=True;
                    BLen:=RomSize-DOff;
                  end;
                  Move(Buff[BOff], TByteArray(Dump)[DOff], BLen);
                end;
              finally
                LocalFree(HLOCAL(Buff));
              end;
          end;
        Result:=Base and Over;
      finally
        LocalFree(HLOCAL(List));
      end;
  end;
end;
{ code by Nico Bendlin - END}


{ based on code by Evgeny Lachinov - BEGIN}
type
  RawSMBIOSData = packed record
    Used20CallingMethod: Byte;
    SMBIOSMajorVersion: Byte;
    SMBIOSMinorVersion: Byte;
    DmiRevision: Byte;
    Length: DWord;
    SMBIOSTableData: array[0..0] of Byte;
  end;
  PRawSMBIOSData = ^RawSMBIOSData;

function DumpRomBiosFW2(RomBase: Pointer; RomSize: Cardinal; out Dump; var Size: Word; var Major,Minor: Byte; var Rev: string): Boolean;
const
  FwTP = $52534D42;  // 'RSMB'
var
  BLen: UINT;
  Buff: PRawSMBIOSData;
  HMod: HMODULE;
begin
  Result:=False;
  HMod:=GetModuleHandle(kernel32);
  if HMod = 0 then
    SetLastError(ERROR_CALL_NOT_IMPLEMENTED)
  else begin
    if not Assigned(GetSystemFirmwareTable) then
      GetSystemFirmwareTable:=GetProcAddress(HMod, 'GetSystemFirmwareTable');
    if not Assigned(GetSystemFirmwareTable) then
      Exit;
    BLen:=GetSystemFirmwareTable(FwTP, 0, nil^, 0);
    if BLen > 0 then begin
      Buff:=PRawSMBIOSData(LocalAlloc(LPTR, BLen));
      if Buff <> nil then try
        BLen:=GetSystemFirmwareTable(FwTP, 0, Buff^, BLen);
        if BLen > 0 then begin
          ResetMemory(Dump, RomSize);
          Size:=Buff^.Length;
          Move(Buff^.SMBIOSTableData, Dump, Size);
          Major:=Buff^.SMBIOSMajorVersion;
          Minor:=Buff^.SMBIOSMinorVersion;
          Rev:=Format('%d.%d',[Lo(Buff^.DmiRevision), Hi(Buff^.DmiRevision)]);
          Result:=True
        end
      finally
        LocalFree(HLOCAL(Buff))
      end
    end
  end
end;

function IsUEFI: Boolean;
begin
  Result:=False;
  if not Assigned(GetFirmwareEnvironmentVariable) then
    GetFirmwareEnvironmentVariable:=GetProcAddress(GetModuleHandle(kernel32), 'GetFirmwareEnvironmentVariableA');
  if Assigned(GetFirmwareEnvironmentVariable) then begin
    GetFirmwareEnvironmentVariable('', '{00000000-0000-0000-0000-000000000000}', nil, 0);
    Result:=GetLastError <> ERROR_INVALID_FUNCTION
  end
end;
{ based on code by Evgeny Lachinov - END}

function DumpRomBiosWMI(RomBase: Pointer; RomSize: Cardinal; out Dump; var Size: Word; var Major,Minor: Byte; var Rev: string): Boolean;
var
  wmi: TInstances;
  sl: TStringList;
  i: Integer;
  b: Byte;
  wmiServices: ISWbemServices;
begin
  Size:=0;
  sl:=TStringList.Create;
  sl.Delimiter:=';';
  WMIConnect('','','','root\WMI',wmiServices);
  try
    WMICommand(wmiServices,'MSSmBios_RawSMBiosTables',wmi);
    if Length(wmi)>0 then begin
      Size:=StrToIntDef(GetInstancePropertyValue(wmi,'Size'),0);
      Major:=StrToIntDef(GetInstancePropertyValue(wmi,'SmbiosMajorVersion'),0);
      Minor:=StrToIntDef(GetInstancePropertyValue(wmi,'SmbiosMinorVersion'),0);
      Rev:=GetInstancePropertyValue(wmi,'DmiRevision');
      sl.DelimitedText:=GetInstancePropertyValue(wmi,'SMBiosData');
      Finalize(wmi);
      ResetMemory(Dump,RomSize);
      for i:=0 to sl.Count-1 do begin
        b:=StrToIntDef(sl[i],0);
        Move(b,TByteArray(Dump)[i],SizeOf(b));
      end;
    end;
  finally
    WMIDisconnect(wmiServices);
    sl.Free;
  end;
  Result:=Size>0;
end;

function GetBIOSCharStr;
var
  i: TSMBIOS_BIOSChar;
begin
  Result:='';
  for i:=Low(TSMBIOS_BIOSChar) to High(TSMBIOS_BIOSChar) do
    if i in Value then
      Result:=Result+BIOSChars[i]+',';
  if Length(Result)>0 then
    SetLength(Result,Length(Result)-1);
end;

function GetMemoryTypeStr(Value: TSMBIOS_MemoryTypes): string;
var
  i: TSMBIOS_MemoryType;
begin
  Result:='';
  for i:=Low(TSMBIOS_MemoryType) to High(TSMBIOS_MemoryType) do
    if i in Value then
      Result:=Result+MemoryTypes[i]+',';
  if Length(Result)>0 then
    SetLength(Result,Length(Result)-1);
end;

function GetMemorySpeedStr(Value: TSMBIOS_MemorySpeeds): string;
var
  i: TSMBIOS_MemorySpeed;
begin
  Result:='';
  for i:=Low(TSMBIOS_MemorySpeed) to High(TSMBIOS_MemorySpeed) do
    if i in Value then
      Result:=Result+MemorySpeeds[i]+',';
  if Length(Result)>0 then
    SetLength(Result,Length(Result)-1);
end;

function GetMemoryVoltageStr(Value: TSMBIOS_Voltages): string;
var
  i: TSMBIOS_Voltage;
begin
  Result:='';
  for i:=Low(TSMBIOS_Voltage) to High(TSMBIOS_Voltage) do
    if i in Value then
      Result:=Result+Voltages[i]+',';
  if Length(Result)>0 then
    SetLength(Result,Length(Result)-1);
end;

function GetMemoryTypeDetailsStr(Value: TSMBIOS_MemoryTypeDetails): string;
var
  i: TSMBIOS_MemoryTypeDetail;
begin
  Result:='';
  for i:=Low(TSMBIOS_MemoryTypeDetail) to High(TSMBIOS_MemoryTypeDetail) do
    if i in Value then
      Result:=Result+MemoryTypeDetails[i]+',';
  if Length(Result)>0 then
    SetLength(Result,Length(Result)-1);
end;

procedure TMiTeC_SMBIOS.AddTable(Addr: Cardinal; Ind,Len: Byte; Hndl: Word; var ST: TSMBIOS_StructTables);
var
  i: Integer;
begin
  SetLength(ST,Length(ST)+1);
  with ST[High(ST)] do begin
    Address:=Addr;
    Indicator:=Ind;
    Length:=Len;
    Handle:=Hndl;
    if Ind>=128 then
      Name:='OEM-specific'
    else
      Name:='Unknown';
    for i:=0 to High(SMB_TableTypes) do
      if SMB_TableTypes[i].Typ=Ind then begin
        if Ind>SMBIOS_OEM_BEGIN then begin
          if (SameText(FSysMan,'HP') or SameText(FSysMan,'HPE') or SameText(FSysMan,'Hewlett-Packard') or SameText(FSysMan,'Hewlett Packard Enterprise')) and (Ind in [SMB_HP_204,SMB_HP_209,SMB_HP_212,SMB_HP_219,SMB_HP_221,SMB_HP_233]) then
            Name:=SMB_TableTypes[i].Name
          else if SameText(FSysMan,'Acer') and (Ind in [SMB_ACER_170]) then
            Name:=SMB_TableTypes[i].Name
          else if (SameText(FSysMan,'IBM') or SameText(FSysMan,'LENOVO')) and (Ind in [SMB_IBM_131,SMB_IBM_135,SMB_IBM_140]) then
            Name:=SMB_TableTypes[i].Name;
        end else
          Name:=SMB_TableTypes[i].Name;
        Break;
      end;
//    MessageBox(0,PAnsiChar(Format('%x: Table %d - %s: Length: %d B',[Address,Indicator,Name,Length])),'',MB_OK);
  end;
end;

procedure TMiTeC_SMBIOS.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub, Sub1: TStructuredStorage;

procedure WriteProcessorToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    WriteStrProperty(sl,'Manufacturer',Self.Processor[AIndex].Manufacturer);
    WriteStrProperty(sl,'Version',Self.Processor[AIndex].Version);
    WriteStrProperty(sl,'Socket',Self.Processor[AIndex].Socket);
    WriteStrProperty(sl,'SerialNumber',Self.Processor[AIndex].SerialNumber);
    WriteStrProperty(sl,'AssetTag',Self.Processor[AIndex].AssetTag);
    WriteStrProperty(sl,'PartNumber',Self.Processor[AIndex].PartNumber);
    WriteIntProperty(sl,'Frequency',Self.Processor[AIndex].Frequency);
    WriteIntProperty(sl,'ExternalClock',Self.Processor[AIndex].ExternalClock);
    WriteIntProperty(sl,'Upgrade',integer(Self.Processor[AIndex].Upgrade));
    WriteDblProperty(sl,'Voltage',Self.Processor[AIndex].Voltage);
    WriteIntProperty(sl,'Typ',integer(Self.Processor[AIndex].Typ));
    WriteIntProperty(sl,'Family',integer(Self.Processor[AIndex].Family));
    WriteIntProperty(sl,'Family2',integer(Self.Processor[AIndex].Family2));
    WriteIntProperty(sl,'CoreCount',integer(Self.Processor[AIndex].CoreCount));
    WriteIntProperty(sl,'CoreEnabled',integer(Self.Processor[AIndex].CoreEnabled));
    WriteIntProperty(sl,'ThreadCount',integer(Self.Processor[AIndex].ThreadCount));
    strm:=Sub1.OpenStream(Format(strm_Proc,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure WriteMemModToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
  mt: TSMBIOS_MemoryType;
  n: integer;
begin
  sl:=TStringList.Create;
  try
    WriteStrProperty(sl,'Socket',Self.MemoryModule[AIndex].Socket);
    WriteIntProperty(sl,'Speed',Self.MemoryModule[AIndex].Speed);
    WriteIntProperty(sl,'Size',Self.MemoryModule[AIndex].Size);
    n:=0;
    for mt:=Low(TSMBIOS_MemoryType) to High(TSMBIOS_MemoryType) do
      if mt in Self.MemoryModule[AIndex].Types then
        n:=n+Round(Power(2,Integer(mt)));
    WriteIntProperty(sl,'Types',n);
    strm:=Sub1.OpenStream(Format(strm_MemMod,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure WriteMemDevToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
  n: integer;
  td: TSMBIOS_MemoryTypeDetail;
begin
  sl:=TStringList.Create;
  try
    WriteStrProperty(sl,'DeviceLocator',Self.MemoryDevice[AIndex].DeviceLocator);
    WriteStrProperty(sl,'BankLocator',Self.MemoryDevice[AIndex].BankLocator);
    WriteStrProperty(sl,'Manufacturer',Self.MemoryDevice[AIndex].Manufacturer);
    WriteStrProperty(sl,'SerialNumber',Self.MemoryDevice[AIndex].SerialNumber);
    WriteStrProperty(sl,'AssetTag',Self.MemoryDevice[AIndex].AssetTag);
    WriteStrProperty(sl,'PartNumber',Self.MemoryDevice[AIndex].PartNumber);
    WriteIntProperty(sl,'Speed',Self.MemoryDevice[AIndex].Speed);
    WriteIntProperty(sl,'Size',Self.MemoryDevice[AIndex].Size);
    WriteIntProperty(sl,'TotalWidth',Self.MemoryDevice[AIndex].TotalWidth);
    WriteIntProperty(sl,'DataWidth',Self.MemoryDevice[AIndex].DataWidth);
    WriteIntProperty(sl,'FormFactor',integer(Self.MemoryDevice[AIndex].FormFactor));
    WriteIntProperty(sl,'Device',integer(Self.MemoryDevice[AIndex].Device));
    n:=0;
    for td:=Low(TSMBIOS_MemoryTypeDetail) to High(TSMBIOS_MemoryTypeDetail) do
      if td in Self.MemoryDevice[AIndex].TypeDetails then
        n:=n+Round(Power(2,Integer(td)));
    WriteIntProperty(sl,'TypeDetails',n);

    WriteIntProperty(sl,'MinimumVoltage',Self.MemoryDevice[AIndex].MinimumVoltage);
    WriteIntProperty(sl,'MaximumVoltage',Self.MemoryDevice[AIndex].MaximumVoltage);
    WriteIntProperty(sl,'ConfiguredVoltage',Self.MemoryDevice[AIndex].ConfiguredVoltage);
    WriteIntProperty(sl,'MemoryTechnology',integer(Self.MemoryDevice[AIndex].MemoryTechnology));
    WriteIntProperty(sl,'OperatingModeCapability',Self.MemoryDevice[AIndex].OperatingModeCapability);
    WriteIntProperty(sl,'FirmwareVersion',Self.MemoryDevice[AIndex].FirmwareVersion);
    WriteIntProperty(sl,'ModuleManufacturerID',Self.MemoryDevice[AIndex].ModuleManufacturerID);
    WriteIntProperty(sl,'ModuleProductID',Self.MemoryDevice[AIndex].ModuleProductID);
    WriteIntProperty(sl,'SubsystemControllerManufacturerID',Self.MemoryDevice[AIndex].SubsystemControllerManufacturerID);
    WriteIntProperty(sl,'SubsystemControllerProductID',Self.MemoryDevice[AIndex].SubsystemControllerProductID);
    WriteIntProperty(sl,'NonVolatileSize',Self.MemoryDevice[AIndex].NonVolatileSize);
    WriteIntProperty(sl,'VolatileSize',Self.MemoryDevice[AIndex].VolatileSize);
    WriteIntProperty(sl,'CacheSize',Self.MemoryDevice[AIndex].CacheSize);
    WriteIntProperty(sl,'LogicalSize',Self.MemoryDevice[AIndex].LogicalSize);

    strm:=Sub1.OpenStream(Format(strm_memDev,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure WritePortToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    WriteStrProperty(sl,'InternalDesignator',Self.Port[AIndex].InternalDesignator);
    WriteStrProperty(sl,'ExternalDesignator',Self.Port[AIndex].ExternalDesignator);
    WriteIntProperty(sl,'InternalConnector',integer(Self.Port[AIndex].InternalConnector));
    WriteIntProperty(sl,'ExternalConnector',integer(Self.Port[AIndex].ExternalConnector));
    WriteIntProperty(sl,'Typ',integer(Self.Port[AIndex].Typ));
    strm:=Sub1.OpenStream(Format(strm_Port,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure WriteSystemSlotToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    WriteStrProperty(sl,'Designation',Self.SystemSlot[AIndex].Designation);
    WriteIntProperty(sl,'Typ',integer(Self.SystemSlot[AIndex].Typ));
    WriteIntProperty(sl,'DataBus',integer(Self.SystemSlot[AIndex].DataBus));
    WriteIntProperty(sl,'ID',Self.SystemSlot[AIndex].ID);
    WriteIntProperty(sl,'Usage',integer(Self.SystemSlot[AIndex].Usage));
    WriteIntProperty(sl,'Length',integer(Self.SystemSlot[AIndex].Length));
    WriteIntProperty(sl,'SegmentGroup',integer(Self.SystemSlot[AIndex].SegmentGroup));
    WriteIntProperty(sl,'BusNumber',integer(Self.SystemSlot[AIndex].BusNumber));
    WriteIntProperty(sl,'DevNumber',integer(Self.SystemSlot[AIndex].DevNumber));
    WriteIntProperty(sl,'FuncNumber',integer(Self.SystemSlot[AIndex].FuncNumber));
    strm:=Sub1.OpenStream(Format(strm_Slot,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure WriteCacheToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    WriteStrProperty(sl,'Designation',Self.Cache[AIndex].Designation);
    WriteIntProperty(sl,'Typ',integer(Self.Cache[AIndex].Typ));
    WriteIntProperty(sl,'SRAMType',integer(Self.Cache[AIndex].SRAMType));
    WriteIntProperty(sl,'MaxSize',Self.Cache[AIndex].MaxSize);
    WriteIntProperty(sl,'InstalledSize',Self.Cache[AIndex].InstalledSize);
    WriteIntProperty(sl,'Speed',Self.Cache[AIndex].Speed);
    WriteIntProperty(sl,'Associativity',integer(Self.Cache[AIndex].Associativity));
    strm:=Sub1.OpenStream(Format(strm_Cache,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure WriteOBDToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    WriteStrProperty(sl,'DeviceName',Self.OnBoardDevice[AIndex].DeviceName);
    WriteIntProperty(sl,'Typ',integer(Self.OnBoardDevice[AIndex].Typ));
    WriteIntProperty(sl,'Status',integer(Self.OnBoardDevice[AIndex].Status));
    strm:=Sub1.OpenStream(Format(strm_OBD,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure WriteOBDXToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    WriteStrProperty(sl,'DeviceName',Self.OnBoardDeviceEx[AIndex].DeviceName);
    WriteIntProperty(sl,'Typ',integer(Self.OnBoardDeviceEx[AIndex].Typ));
    WriteIntProperty(sl,'Status',integer(Self.OnBoardDeviceEx[AIndex].Status));
    WriteIntProperty(sl,'Instance',integer(Self.OnBoardDeviceEx[AIndex].Instance));
    WriteIntProperty(sl,'SegmentGroupNumber',integer(Self.OnBoardDeviceEx[AIndex].SegmentGroupNumber));
    WriteIntProperty(sl,'BusNumber',integer(Self.OnBoardDeviceEx[AIndex].BusNumber));
    WriteIntProperty(sl,'DeviceNumber',integer(Self.OnBoardDeviceEx[AIndex].DeviceNumber));
    WriteIntProperty(sl,'FunctionNumber',integer(Self.OnBoardDeviceEx[AIndex].FunctionNumber));
    strm:=Sub1.OpenStream(Format(strm_OBDX,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure WriteTempProbeToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    WriteStrProperty(sl,'Description',Self.TemperatureProbe[AIndex].Description);
    WriteIntProperty(sl,'Location',integer(Self.TemperatureProbe[AIndex].Location));
    WriteIntProperty(sl,'Status',integer(Self.TemperatureProbe[AIndex].Status));
    WriteIntProperty(sl,'Max',Self.TemperatureProbe[AIndex].Max);
    WriteIntProperty(sl,'Min',Self.TemperatureProbe[AIndex].Min);
    WriteIntProperty(sl,'Resolution',Self.TemperatureProbe[AIndex].Resolution);
    WriteIntProperty(sl,'Tolerance',Self.TemperatureProbe[AIndex].Tolerance);
    WriteIntProperty(sl,'Accuracy',Self.TemperatureProbe[AIndex].Accuracy);
    WriteIntProperty(sl,'NominalValue',Self.TemperatureProbe[AIndex].NominalValue);
    strm:=Sub1.OpenStream(Format(strm_TempProbe,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure WriteVoltProbeToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    WriteStrProperty(sl,'Description',Self.VoltageProbe[AIndex].Description);
    WriteIntProperty(sl,'Location',integer(Self.VoltageProbe[AIndex].Location));
    WriteIntProperty(sl,'Status',integer(Self.VoltageProbe[AIndex].Status));
    WriteIntProperty(sl,'Max',Self.VoltageProbe[AIndex].Max);
    WriteIntProperty(sl,'Min',Self.VoltageProbe[AIndex].Min);
    WriteIntProperty(sl,'Resolution',Self.VoltageProbe[AIndex].Resolution);
    WriteIntProperty(sl,'Tolerance',Self.VoltageProbe[AIndex].Tolerance);
    WriteIntProperty(sl,'Accuracy',Self.VoltageProbe[AIndex].Accuracy);
    WriteIntProperty(sl,'NominalValue',Self.VoltageProbe[AIndex].NominalValue);
    strm:=Sub1.OpenStream(Format(strm_VoltProbe,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure WriteCurrProbeToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    WriteStrProperty(sl,'Description',Self.CurrentProbe[AIndex].Description);
    WriteIntProperty(sl,'Location',integer(Self.CurrentProbe[AIndex].Location));
    WriteIntProperty(sl,'Status',integer(Self.CurrentProbe[AIndex].Status));
    WriteIntProperty(sl,'Max',Self.CurrentProbe[AIndex].Max);
    WriteIntProperty(sl,'Min',Self.CurrentProbe[AIndex].Min);
    WriteIntProperty(sl,'Resolution',Self.CurrentProbe[AIndex].Resolution);
    WriteIntProperty(sl,'Tolerance',Self.CurrentProbe[AIndex].Tolerance);
    WriteIntProperty(sl,'Accuracy',Self.CurrentProbe[AIndex].Accuracy);
    WriteIntProperty(sl,'NominalValue',Self.CurrentProbe[AIndex].NominalValue);
    strm:=Sub1.OpenStream(Format(strm_CurrProbe,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure WriteCoolDevToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    WriteStrProperty(sl,'Description',Self.CoolingDevice[AIndex].Description);
    WriteIntProperty(sl,'Type',integer(Self.CoolingDevice[AIndex].Typ));
    WriteIntProperty(sl,'Status',integer(Self.CoolingDevice[AIndex].Status));
    WriteIntProperty(sl,'GroupUnit',Self.CoolingDevice[AIndex].GroupUnit);
    WriteIntProperty(sl,'TPHandle',Self.CoolingDevice[AIndex].TemperatureProbeHandle);
    WriteIntProperty(sl,'Speed',Self.CoolingDevice[AIndex].NominalSpeed);
    strm:=Sub1.OpenStream(Format(strm_CoolDev,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure WriteTPMDevToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    WriteStrProperty(sl,'VendorID',Self.TPMDevice[AIndex].VendorID);
    WriteIntProperty(sl,'MajorSpecVersion',Self.TPMDevice[AIndex].MajorSpecVersion);
    WriteIntProperty(sl,'MinorSpecVersion',Self.TPMDevice[AIndex].MinorSpecVersion);
    WriteIntProperty(sl,'FirmwareVersion1',Self.TPMDevice[AIndex].FirmwareVersion1);
    WriteIntProperty(sl,'FirmwareVersion2',Self.TPMDevice[AIndex].FirmwareVersion2);
    WriteStrProperty(sl,'Description',Self.TPMDevice[AIndex].Description);
    WriteIntProperty(sl,'Chars',Self.TPMDevice[AIndex].Chars);
    WriteIntProperty(sl,'OEMDef',Self.TPMDevice[AIndex].OEMDef);
    strm:=Sub1.OpenStream(Format(strm_TPMD,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure WritePortableBatteryToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    WriteStrProperty(sl,'Location',Self.Battery[AIndex].Location);
    WriteStrProperty(sl,'Manufacturer',Self.Battery[AIndex].Manufacturer);
    WriteStrProperty(sl,'ManufacturerDate',Self.Battery[AIndex].ManufacturerDate);
    WriteStrProperty(sl,'SerialNumber',Self.Battery[AIndex].SerialNumber);
    WriteStrProperty(sl,'DeviceName',Self.Battery[AIndex].DeviceName);
    WriteIntProperty(sl,'DeviceChemistry',integer(Self.Battery[AIndex].DeviceChemistry));
    WriteIntProperty(sl,'DesignCapacity',Self.Battery[AIndex].DesignCapacity);
    WriteIntProperty(sl,'DesignVoltage',Self.Battery[AIndex].DesignVoltage);
    WriteIntProperty(sl,'MaxErrorInBatData',Self.Battery[AIndex].MaxErrorInBatData);
    WriteStrProperty(sl,'SBDSVersionNumber',Self.Battery[AIndex].SBDSVersionNumber);
    WriteStrProperty(sl,'SBDSDeviceChemistry',Self.Battery[AIndex].SBDSDeviceChemistry);
    WriteIntProperty(sl,'SBDSSerialNumber',Self.Battery[AIndex].SBDSSerialNumber);
    WriteIntProperty(sl,'SBDSManufactureDate',Self.Battery[AIndex].SBDSManufactureDate);
    WriteIntProperty(sl,'DesignCapacityMultiplier',Self.Battery[AIndex].DesignCapacityMultiplier);
    WriteIntProperty(sl,'OEMSpec',Self.Battery[AIndex].OEMSpec);
    strm:=Sub1.OpenStream(Format(strm_BAT,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

var
  i: Integer;
  strm: TStorageStream;
  sl: TStringList;
  n: integer;
  ms: TSMBIOS_MemorySpeed;
  mt: TSMBIOS_MemoryType;
  mv: TSMBIOS_Voltage;
  bc: TSMBIOS_BIOSChar;
begin
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(StorageFolderName);
    Sub:=SS.OpenSubStorage(StorageFolderName,STG_OPEN,True);
    try
      sl:=TStringList.Create;
      try
        WriteIntProperty(sl,'RevisionMajor',Self.RevisionMajor);
        WriteIntProperty(sl,'RevisionMinor',Self.RevisionMinor);
        WriteIntProperty(sl,'MajorVersion',Self.MajorVersion);
        WriteIntProperty(sl,'MinorVersion',Self.MinorVersion);
        WriteIntProperty(sl,'SMBIOSAddress',Self.SMBIOSAddress);
        WriteIntProperty(sl,'StructStart',Self.StructStart);
        WriteIntProperty(sl,'StructLength',Self.StructLength);
        WriteIntProperty(sl,'StructCount',Self.StructCount);
        WriteStrProperty(sl,'BIOSVendor',Self.BIOSVendor);
        WriteStrProperty(sl,'BIOSVersion',Self.BIOSVersion);
        WriteStrProperty(sl,'BIOSDate',Self.BIOSDate);
        WriteIntProperty(sl,'BIOSSize',Self.BIOSSize);
        WriteIntProperty(sl,'BIOSMajorVersion',Self.BIOSMajorVersion);
        WriteIntProperty(sl,'BIOSMinorVersion',Self.BIOSMinorVersion);
        WriteIntProperty(sl,'BIOSECFMajorVersion',Self.BIOS_ECF_MajorVersion);
        WriteIntProperty(sl,'BIOSECFMinorVersion',Self.BIOS_ECF_MinorVersion);
        WriteIntProperty(sl,'BIOSECFMajorVersion',Self.BIOS_ECF_MajorVersion);
        WriteIntProperty(sl,'UEFI',Integer(Self.FUEFI));
        n:=0;
        for bc:=Low(TSMBIOS_BIOSChar) to High(TSMBIOS_BIOSChar) do
          if bc in Self.BIOSCharacteristics then
            n:=n+Round(Power(2,Integer(bc)));
        WriteIntProperty(sl,'BIOSCharacteristics',n);
        WriteStrProperty(sl,'SystemModel',Self.SystemModel);
        WriteStrProperty(sl,'SystemManufacturer',Self.SystemManufacturer);
        WriteStrProperty(sl,'SystemVersion',Self.SystemVersion);
        WriteStrProperty(sl,'SystemSerial',Self.SystemSerial);
        WriteStrProperty(sl,'SystemUUID',Self.SystemUUID);
        WriteStrProperty(sl,'MainboardModel',Self.MainBoardModel);
        WriteStrProperty(sl,'MainboardManufacturer',Self.MainBoardManufacturer);
        WriteStrProperty(sl,'MainboardVersion',Self.MainBoardVersion);
        WriteStrProperty(sl,'MainboardSerial',Self.MainBoardSerial);
        WriteStrProperty(sl,'MainboardAssetTag',Self.MainBoardAssetTag);
        WriteStrProperty(sl,'MainboardLocationInChassis',Self.MainBoardLocationInChassis);
        WriteIntProperty(sl,'ChassisModel',integer(Self.ChassisModel));
        WriteStrProperty(sl,'ChassisManufacturer',Self.ChassisManufacturer);
        WriteStrProperty(sl,'ChassisVersion',Self.ChassisVersion);
        WriteStrProperty(sl,'ChassisSerial',Self.ChassisSerial);
        WriteStrProperty(sl,'ChassisAssetTag',Self.ChassisAssetTag);
        WriteIntProperty(sl,'MemCtrlCurrentInterleave',integer(Self.MemCtrlCurrentInterleave));
        WriteIntProperty(sl,'MemCtrlSupportedInterleave',integer(Self.MemCtrlSupportedInterleave));
        n:=0;
        for ms:=Low(TSMBIOS_MemorySpeed) to High(TSMBIOS_MemorySpeed) do
          if ms in Self.MemCtrlSupportedSpeeds then
            n:=n+Round(Power(2,Integer(ms)));
        WriteIntProperty(sl,'MemCtrlSupportedSpeeds',n);
        n:=0;
        for mt:=Low(TSMBIOS_MemoryType) to High(TSMBIOS_MemoryType) do
          if mt in Self.MemCtrlSupportedTypes then
            n:=n+Round(Power(2,Integer(mt)));
        WriteIntProperty(sl,'MemCtrlSupportedTypes',n);
        n:=0;
        for mv:=Low(TSMBIOS_Voltage) to High(TSMBIOS_Voltage) do
          if mv in Self.MemCtrlSupportedVoltages then
            n:=n+Round(Power(2,Integer(mv)));
        WriteIntProperty(sl,'MemCtrlSupportedVoltages',n);
        WriteIntProperty(sl,'MemCtrlMaxSize',MemCtrlMaxSize);
        WriteIntProperty(sl,'MemCtrlSlotCount',MemCtrlSlotCount);

        WriteIntProperty(sl,'PMALocation',integer(Self.PMALocation));
        WriteIntProperty(sl,'PMAUse',integer(Self.PMAUse));
        WriteIntProperty(sl,'PMAErrorCorrectionType',integer(Self.PMAErrorCorrectionType));
        WriteIntProperty(sl,'PMAMaximumCapacity',Self.PMAMaximumCapacity);
        WriteIntProperty(sl,'PMANumberOfMemoryDevices',Self.PMANumberOfMemoryDevices);

        WriteIntProperty(sl,'SystemPowerSupplyPowerUnitGroup',Self.SystemPowerSupplyPowerUnitGroup);
        WriteStrProperty(sl,'SystemPowerSupplyLocation',Self.SystemPowerSupplyLocation);
        WriteStrProperty(sl,'SystemPowerSupplyDeviceName',Self.SystemPowerSupplyDeviceName);
        WriteStrProperty(sl,'SystemPowerSupplyManufacturer',Self.SystemPowerSupplyManufacturer);
        WriteStrProperty(sl,'SystemPowerSupplySerialNumber',Self.SystemPowerSupplySerialNumber);
        WriteStrProperty(sl,'SystemPowerSupplyAssetTagNumber',Self.SystemPowerSupplyAssetTagNumber);
        WriteStrProperty(sl,'SystemPowerSupplyModelPartNumber',Self.SystemPowerSupplyModelPartNumber);
        WriteStrProperty(sl,'SystemPowerSupplyRevisionLevel',Self.SystemPowerSupplyRevisionLevel);
        WriteIntProperty(sl,'SystemPowerSupplyMaxPowerCapacity',Self.SystemPowerSupplyMaxPowerCapacity);
        WriteIntProperty(sl,'SystemPowerSupplyChars',Self.SystemPowerSupplyChars);
        WriteIntProperty(sl,'SystemPowerSupplyInputVoltageProbeHandle',Self.SystemPowerSupplyInputVoltageProbeHandle);
        WriteIntProperty(sl,'SystemPowerSupplyCoolingDeviceHandle',Self.SystemPowerSupplyCoolingDeviceHandle);
        WriteIntProperty(sl,'SystemPowerSupplyInputCurrenyProbeHandle',Self.SystemPowerSupplyInputCurrenyProbeHandle);

        strm:=Sub.OpenStream(strm_Props,STG_OPEN,True);
        try
          SaveToEncodedStream(sl,strm,ACodeStream);
        finally
          strm.Free;
        end;
      finally
        sl.Free;
      end;

      Sub1:=Sub.OpenSubStorage(Processor_StorageFolderName,STG_OPEN,True);
      try
        for i:=0 to Self.ProcessorCount-1 do
          WriteProcessorToStream(i);
      finally
        Sub1.Free;
      end;
      Sub1:=Sub.OpenSubStorage(MemoryModule_StorageFolderName,STG_OPEN,True);
      try
        for i:=0 to Self.MemoryModuleCount-1 do
          WriteMemModToStream(i);
      finally
        Sub1.Free;
      end;
      Sub1:=Sub.OpenSubStorage(MemoryDevice_StorageFolderName,STG_OPEN,True);
      try
        for i:=0 to Self.MemoryDeviceCount-1 do
          WriteMemDevToStream(i);
      finally
        Sub1.Free;
      end;
      Sub1:=Sub.OpenSubStorage(Port_StorageFolderName,STG_OPEN,True);
      try
        for i:=0 to Self.PortCount-1 do
          WritePortToStream(i);
      finally
        Sub1.Free;
      end;
      Sub1:=Sub.OpenSubStorage(SystemSlot_StorageFolderName,STG_OPEN,True);
      try
        for i:=0 to Self.SystemSlotCount-1 do
          WriteSystemSlotToStream(i);
      finally
        Sub1.Free;
      end;
      Sub1:=Sub.OpenSubStorage(Cache_StorageFolderName,STG_OPEN,True);
      try
        for i:=0 to Self.CacheCount-1 do
          WriteCacheToStream(i);
      finally
        Sub1.Free;
      end;
      Sub1:=Sub.OpenSubStorage(OnBoardDevice_StorageFolderName,STG_OPEN,True);
      try
        for i:=0 to Self.OnBoardDeviceCount-1 do
          WriteOBDToStream(i);
      finally
        Sub1.Free;
      end;
      Sub1:=Sub.OpenSubStorage(OnBoardDeviceEx_StorageFolderName,STG_OPEN,True);
      try
        for i:=0 to Self.OnBoardDeviceExCount-1 do
          WriteOBDXToStream(i);
      finally
        Sub1.Free;
      end;
      Sub1:=Sub.OpenSubStorage(TemperatureProbe_StorageFolderName,STG_OPEN,True);
      try
        for i:=0 to Self.TemperatureProbeCount-1 do
          WriteTempProbeToStream(i);
      finally
        Sub1.Free;
      end;
      Sub1:=Sub.OpenSubStorage(VoltageProbe_StorageFolderName,STG_OPEN,True);
      try
        for i:=0 to Self.VoltageProbeCount-1 do
          WriteVoltProbeToStream(i);
      finally
        Sub1.Free;
      end;
      Sub1:=Sub.OpenSubStorage(CurrentProbe_StorageFolderName,STG_OPEN,True);
      try
        for i:=0 to Self.CurrentProbeCount-1 do
          WriteCurrProbeToStream(i);
      finally
        Sub1.Free;
      end;
      Sub1:=Sub.OpenSubStorage(CoolingDevice_StorageFolderName,STG_OPEN,True);
      try
        for i:=0 to Self.CoolingDeviceCount-1 do
          WriteCoolDevToStream(i);
      finally
        Sub1.Free;
      end;
      Sub1:=Sub.OpenSubStorage(TPMDevice_StorageFolderName,STG_OPEN,True);
      try
        for i:=0 to Self.TPMDeviceCount-1 do
          WriteTPMDevToStream(i);
      finally
        Sub1.Free;
      end;
      Sub1:=Sub.OpenSubStorage(PortableBattery_StorageFolderName,STG_OPEN,True);
      try
        for i:=0 to Self.BatteryCount-1 do
          WritePortableBatteryToStream(i);
      finally
        Sub1.Free;
      end;
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_SMBIOS.ScanTables;
var
 l,sl: Byte;
 p,i: Cardinal;
 Found: Boolean;
begin
  p:=0;
  with FStructure do begin
  if not IsValidAddress(FStructStart) then
    Exit;
  AddTable(FStructStart+p,ByteValue[FStructStart+p],ByteValue[FStructStart+p+1],WordValue[FStructStart+p+2],StructTables);
  Found:=ByteValue[FStructStart+p]=0;
  repeat
    sl:=ByteValue[FStructStart+p+1];
    p:=p+sl+1;
    i:=0;
    l:=ByteValue[FStructStart+p-1];
// Handle Memory Controller Information specially as some BIOSes (Award Modular BIOS v4.51PG, P6BX-A+ Ver 3.2c, 11/28/1998) don't put two zero bytes after it
    if not((StructTables[High(StructTables)].Indicator=5) and (l>5) and (l<>32)) then begin
      while IsvalidAddress(FStructStart+p+i) and ((l+ByteValue[FStructStart+p+i]<>0) or (Found and (ByteValue[FStructStart+p+i+1]=0))) do begin
        l:=ByteValue[FStructStart+p+i];
        Inc(i);
      end;
      p:=p+i+1;
    end else
      p:=p-1;
    //if Length(StructTables)<StructCount then
      AddTable(FStructStart+p,ByteValue[FStructStart+p],ByteValue[FStructStart+p+1],WordValue[FStructStart+p+2],StructTables);
    Found:=Found or (ByteValue[FStructStart+p]=0);
  until (ByteValue[FStructStart+p]=SMB_EOT) or (FStructStart+p>=FStructStart+FLen);
  end;
end;

procedure TMiTeC_SMBIOS.SetBat(Index: Byte; const Value: TSMBIOS_Battery);
begin
  if Index>High(FBat) then begin
    SetLength(FBat,Length(FBat)+1);
    Index:=High(FBat);
  end;
  FBat[Index]:=Value;
end;

procedure TMiTeC_SMBIOS.SetCache(Index: Byte; const Value: TSMBIOS_Cache);
begin
  if Index>High(FCache) then begin
    SetLength(FCache,Length(FCache)+1);
    Index:=High(FCache);
  end;
  FCache[Index]:=Value;
end;

procedure TMiTeC_SMBIOS.SetCoolDev(Index: Byte; const Value: TSMBIOS_CoolingDevice);
begin
  if Index>High(FCD) then begin
    SetLength(FCD,Length(FCD)+1);
    Index:=High(FCD);
  end;
  FCD[Index]:=Value;
end;

procedure TMiTeC_SMBIOS.SetCurrProbe(Index: Byte; const Value: TSMBIOS_CurrentProbe);
begin
  if Index>High(FCP) then begin
    SetLength(FCP,Length(FCP)+1);
    Index:=High(FCP);
  end;
  FCP[Index]:=Value;
end;

procedure TMiTeC_SMBIOS.SetHP209(Index: Byte; const Value: TSMBIOS_HP_221);
begin
  if Index>High(FHP209) then begin
    SetLength(FHP209,Length(FHP209)+1);
    Index:=High(FHP209);
  end;
  FHP209[Index]:=Value;
end;

procedure TMiTeC_SMBIOS.SetHP221(Index: Byte; const Value: TSMBIOS_HP_221);
begin
  if Index>High(FHP221) then begin
    SetLength(FHP221,Length(FHP221)+1);
    Index:=High(FHP221);
  end;
  FHP221[Index]:=Value;
end;

procedure TMiTeC_SMBIOS.SetMemoryDevice(Index: Byte;
  const Value: TSMBIOS_MemoryDevice);
begin
  if Index>High(FMemDev) then begin
    SetLength(FMemDev,Length(FMemDev)+1);
    Index:=High(FMemDev);
  end;
  FMemDev[Index]:=Value;
end;

procedure TMiTeC_SMBIOS.SetMemoryModule(Index: Byte;
  const Value: TSMBIOS_MemoryModule);
begin
  if Index>High(FMemMod) then begin
    SetLength(FMemMod,Length(FMemMod)+1);
    Index:=High(FMemMod);
  end;
  FMemMod[Index]:=Value;
end;

procedure TMiTeC_SMBIOS.SetOBD(Index: Byte; const Value: TSMBIOS_OnBoardDevice);
begin
  if Index>High(FOBD) then begin
    SetLength(FOBD,Length(FOBD)+1);
    Index:=High(FOBD);
  end;
  FOBD[Index]:=Value;
end;

procedure TMiTeC_SMBIOS.SetOBDX(Index: Byte;
  const Value: TSMBIOS_OnBoardDeviceEx);
begin
  if Index>High(FOBDX) then begin
    SetLength(FOBDX,Length(FOBDX)+1);
    Index:=High(FOBDX);
  end;
  FOBDX[Index]:=Value;
end;

procedure TMiTeC_SMBIOS.SetPort(Index: Byte; const Value: TSMBIOS_Port);
begin
  if Index>High(FPort) then begin
    SetLength(FPort,Length(FPort)+1);
    Index:=High(FPort);
  end;
  FPort[Index]:=Value;
end;

procedure TMiTeC_SMBIOS.SetTempProbe(Index: Byte;
  const Value: TSMBIOS_TemperatureProbe);
begin
  if Index>High(FTP) then begin
    SetLength(FTP,Length(FTP)+1);
    Index:=High(FTP);
  end;
  FTP[Index]:=Value;
end;

procedure TMiTeC_SMBIOS.SetTPMD(Index: Byte; const Value: TSMBIOS_TPMDevice);
begin
  if Index>High(FTPM) then begin
    SetLength(FTPM,Length(FTPM)+1);
    Index:=High(FTPM);
  end;
  FTPM[Index]:=Value;
end;

procedure TMiTeC_SMBIOS.SetVoltProbe(Index: Byte; const Value: TSMBIOS_VoltageProbe);
begin
  if Index>High(FVP) then begin
    SetLength(FVP,Length(FVP)+1);
    Index:=High(FVP);
  end;
  FVP[Index]:=Value;
end;

procedure TMiTeC_SMBIOS.SetProc(Index: Byte; const Value: TSMBIOS_Processor);
begin
  if Index>High(FProc) then begin
    SetLength(FProc,Length(FProc)+1);
    Index:=High(FProc);
  end;
  FProc[Index]:=Value;
end;

procedure TMiTeC_SMBIOS.SetSlot(Index: Byte; const Value: TSMBIOS_Slot);
begin
  if Index>High(FSlot) then begin
    SetLength(FSlot,Length(FSlot)+1);
    Index:=High(FSlot);
  end;
  FSlot[Index]:=Value;
end;

function TMiTeC_SMBIOS.FindTableRecord;
var
  i: integer;
begin
  Finalize(Result);
  ResetMemory(Result,SizeOf(Result));
  for i:=From to High(ST) do
    if ST[i].Indicator=AType then begin
      Result:=ST[i];
      Break;
    end;
end;

function TMiTeC_SMBIOS.FindTableIndex;
var
  i: integer;
begin
  Result:=-1;
  for i:=From to High(ST) do
    if ST[i].Indicator=AType then begin
      Result:=i;
      Break;
    end;
end;

procedure TMiTeC_SMBIOS.RefreshData;
const
  smbios_as: AnsiString = '_SM_';
  dmi_as: AnsiString = '_DMI_';
var
  Buffer: TArrayBuffer;
  idx,j,c: Integer;
  sl,b: Byte;
  i,p,blockptr: Cardinal;
  ok: Boolean;
  w: Word;
  buf: TRomBiosDump;
  m: PAnsiChar;
  fw: Boolean;
  wmi: Boolean;
  q: Int64;
  s: string;
begin
  inherited;

  Clear;
  fw:=False;
  blockptr:=RomBiosDumpBase;
  wmi:=False;

  try
    if FLocal then begin
      FUEFI:=IsUEFI;
      repeat
        ok:=False;
        if not fw then
          ok:=DumpRomBiosFw2(RomBiosDumpBasePtr,RomBiosDumpSize,Buf,FLen,FMajorVersion,FMinorVersion,s) or DumpRomBiosFw(RomBiosDumpBasePtr,RomBiosDumpSize,Buf);
        if ok then begin
          FSMBIOS.StartAddress:=0;
          FSMBIOS.EndAddress:=RomBiosDumpSize-1;
          FSMBIOS.MemorySize:=RomBiosDumpSize;
          p:=Pos('.',s);
          if p>0 then begin
            FRevisionMajor:=StrToIntDef(Copy(s,1,p-1),0);
            FRevisionMinor:=StrToIntDef(Copy(s,p+1),0);
          end;
          m:=AllocMem(SizeOf(buf.ByteArray));
          try
            Move(buf.ByteArray[0],m[0],SizeOf(buf.ByteArray));
            FSMBIOS.LoadFromMemory(m,0,SizeOf(buf.ByteArray));
          finally
            FreeMem(m);
          end;
          fw:=True;
        end;
        if not ok and not wmi and not FNoWMI then begin
          ok:=DumpRomBiosWMI(RomBiosDumpBasePtr,RomBiosDumpSize,Buf,FLen,FMajorVersion,FMinorVersion,s);
          if ok then begin
            FSMBIOS.StartAddress:=0;
            FSMBIOS.EndAddress:=RomBiosDumpSize-1;
            FSMBIOS.MemorySize:=RomBiosDumpSize;
            p:=Pos('.',s);
            if p>0 then begin
              FRevisionMajor:=StrToIntDef(Copy(s,1,p-1),0);
              FRevisionMinor:=StrToIntDef(Copy(s,p+1),0);
            end;
            m:=AllocMem(SizeOf(buf.ByteArray));
            try
              Move(buf.ByteArray[0],m[0],SizeOf(buf.ByteArray));
              FSMBIOS.LoadFromMemory(m,0,SizeOf(buf.ByteArray));
            finally
              FreeMem(m);
            end;
            wmi:=True;
          end;
        end;
        if not ok then begin
          FSMBIOS.StartAddress:=blockptr;
          FSMBIOS.EndAddress:=blockptr+RomBiosBlockSize;
          FSMBIOS.MemorySize:=RomBiosBlockSize;
          FSMBIOS.RefreshData;
        end;

        ok:=Assigned(FSMBIOS.Memory);
        if ok then
          for i:=FSMBIOS.StartAddress to FSMBIOS.EndAddress do begin
            if (FSMBIOS.CharValue[i]=smbios_as[1]) then begin
              ResetMemory(Buffer,Sizeof(Buffer));
              Buffer:=FSMBIOS.ArrayValue[i,Length(smbios_as)];
              if (Pos(string(smbios_as),string(Buffer))=1) then begin
                Buffer:=FSMBIOS.ArrayValue[i+16,Length(dmi_as)];
                if (Pos(string(dmi_as),string(Buffer))=1) then begin
                  FStart:=i;
                  Break;
                end;
              end;
            end;
          end;
        if (FStart=0) then
          for i:=FSMBIOS.StartAddress to FSMBIOS.EndAddress do begin
            if (FSMBIOS.CharValue[i]=dmi_as[1]) then begin
              ResetMemory(Buffer,SizeOf(Buffer));
              Buffer:=FSMBIOS.ArrayValue[i,Length(dmi_as)];
              if Pos(string(dmi_as),string(Buffer))=1 then begin
                FStart:=i-$10;
                Break;
              end;
            end;
          end;
        Inc(blockptr,RomBiosBlockSize+1);
      until (FStart>0) or (blockptr>RomBiosDumpEnd) or wmi;

      if (FStart>0) or wmi then begin
        if not wmi then begin
          FMinorVersion:=FSMBIOS.ByteValue[FStart+$7];
          FMajorVersion:=FSMBIOS.ByteValue[FStart+$6];
          s:=IntToHex(FSMBIOS.ByteValue[FStart+$1E],2);
          FRevisionMajor:=StrToIntDef(s[1],0);
          FRevisionMinor:=StrToIntDef(s[2],0);
          FStructStart:=FSMBIOS.DWORDValue[FStart+$18];
          FTableCount:=FSMBIOS.WORDValue[FStart+$1C];
          FLen:=FSMBIOS.WORDValue[FStart+$16];
        end;
        if (FStructStart<=0) and not wmi then
          Exit;
        FStructure.StartAddress:=FStructStart;
        FStructure.MemorySize:=FLen;
        if fw or wmi then begin
          if not wmi then begin
            FStructStart:=FStructStart-RomBiosDumpBase;
            if FStructStart>=FSMBIOS.MemorySize then
              FStructStart:=FStart+$1F;
          end;
          FStructure.MemorySize:=FLen;
          FStructure.StartAddress:=0;
          FStructure.LoadFromMemory(FSMBIOS.Memory,FStructStart,FLen);
          FStructStart:=0;
        end else
          FStructure.RefreshData;
      end;
    end else begin
      if FStructure.MemorySize=0 then begin
        for i:=FSMBIOS.StartAddress to FSMBIOS.EndAddress do begin
          if (FSMBIOS.CharValue[i]=smbios_as[1]) then begin
            ResetMemory(Buffer,Sizeof(Buffer));
            Buffer:=FSMBIOS.ArrayValue[i,Length(smbios_as)];
            if (Pos(string(smbios_as),string(Buffer))=1) then begin
              Buffer:=FSMBIOS.ArrayValue[i+16,Length(dmi_as)];
              if (Pos(string(dmi_as),string(Buffer))=1) then begin
                FStart:=i;
                Break;
              end;
            end;
          end;
        end;
        if (FStart=0) then
          for i:=FSMBIOS.StartAddress to FSMBIOS.EndAddress do begin
            if (FSMBIOS.CharValue[i]=dmi_as[1]) then begin
              ResetMemory(Buffer,SizeOf(Buffer));
              Buffer:=FSMBIOS.ArrayValue[i,Length(dmi_as)];
              if Pos(string(dmi_as),string(Buffer))=1 then begin
                FStart:=i-$10;
                Break;
              end;
            end;
          end;
        if (FStart>0) then begin
          FMinorVersion:=FSMBIOS.ByteValue[FStart+$7];
          FMajorVersion:=FSMBIOS.ByteValue[FStart+$6];
          s:=IntToHex(FSMBIOS.ByteValue[FStart+$1E],2);
          FRevisionMajor:=StrToIntDef(s[1],0);
          FRevisionMinor:=StrToIntDef(s[2],0);
          FStructStart:=FSMBIOS.DWORDValue[FStart+$18];
          FTableCount:=FSMBIOS.WORDValue[FStart+$1C];
          FLen:=FSMBIOS.WORDValue[FStart+$16];
        end;
        if (FStructStart<=0) then
          FStructStart:=0;
        FStructure.StartAddress:=FStructStart;
        if FStructStart>0 then
          FStructStart:=FStructStart-RomBiosDumpBase;
        if FStructStart>=FSMBIOS.MemorySize then
          FStructStart:=FStart+$1F;
        if FLen=0 then
          FLen:=Min(MaxWord,FSMBIOS.MemorySize);
        FStructure.MemorySize:=FLen;
        FStructure.StartAddress:=0;
        FStructure.LoadFromMemory(FSMBIOS.Memory,FStructStart,FLen);
        FStructStart:=0;
        ok:=True;
      end else begin
        if SameText(FStructure.StringValue[0,1],'SMBIOS') then begin
          FMinorVersion:=FStructure.ByteValue[$8];
          FMajorVersion:=FStructure.ByteValue[$7];
          s:=IntToHex(FStructure.ByteValue[FStart+$9],2);
          FRevisionMajor:=StrToIntDef(s[1],0);
          FRevisionMinor:=StrToIntDef(s[2],0);
          FTableCount:=FStructure.WORDValue[FStart+$A];
          FStructStart:=16;
        end else begin
          FStructStart:=0;
          FTableCount:=MaxWord;
        end;
        FLen:=FStructure.MemorySize;
        ok:=True;
      end;
    end;

    if ok then begin
      ScanTables;

      with FStructure do begin
        // Table 0
        p:=FindTableRecord(SMB_BIOSINFO,StructTables).Address;
        if (p>=FStructStart) and (ByteValue[p]=SMB_BIOSINFO) then begin
          sl:=ByteValue[p+1];
          FBIOSSize:=(ByteValue[p+9]+1)*64;
          q:=QWORDValue[p+$A];
          if not IsBitOn(q,3) then begin
            for i:=4 to 19 do
              if IsBitOn(q,i) then
                Include(FBIOSChars,TSMBIOS_BIOSChar(i-4));
            if IsBitOn(q,31) then
              Include(FBIOSChars,smbcNECPC98);
          end;
          if ByteValue[p+1]-$12>0 then begin
            b:=ByteValue[p+$12];
            if IsBitOn(b,0) then
              Include(FBIOSChars,smbcACPI);
            if IsBitOn(b,1) then
              Include(FBIOSChars,smbcUSBLegacy);
            if IsBitOn(b,2) then
              Include(FBIOSChars,smbcAGP);
            if IsBitOn(b,3) then
              Include(FBIOSChars,smbcI2OBoot);
            if IsBitOn(b,4) then
              Include(FBIOSChars,smbcLS120Boot);
            if IsBitOn(b,5) then
              Include(FBIOSChars,smbcATAPIZIPDriveBoot);
            if IsBitOn(b,6) then
              Include(FBIOSChars,smbcIEE1394Boot);
            if IsBitOn(b,7) then
              Include(FBIOSChars,smbcSmartBattery);
          end;
          if ByteValue[p+1]-$12>1 then begin
            b:=ByteValue[p+$13];
            if IsBitOn(b,0) then
              Include(FBIOSChars,smbcBIOSBootSpec);
            if IsBitOn(b,1) then
              Include(FBIOSChars,smbcFKINSBoot);
            if IsBitOn(b,2) then
              Include(FBIOSChars,smbcTCD);
            if IsBitOn(b,3) then
              Include(FBIOSChars,smbcUEFISpec);
            if IsBitOn(b,4) then
              Include(FBIOSChars,smbcVM);
          end;
          FBIOSMajor:=ByteValue[p+$14];
          FBIOSMinor:=ByteValue[p+$15];
          FBIOSECFMajor:=ByteValue[p+$16];
          FBIOSECFMinor:=ByteValue[p+$17];
          if ByteValue[p+4]>0 then
            FBIOSVendor:=StringValue[p+sl,ByteValue[p+4]];
          if ByteValue[p+5]>0 then
            FBIOSVersion:=StringValue[p+sl,ByteValue[p+5]];
          if ByteValue[p+8]>0 then
            FBIOSDate:=StringValue[p+sl,ByteValue[p+8]];
        end;

        // Table 1
        idx:=FindTableIndex(SMB_SYSINFO,StructTables);
        p:=FindTableRecord(SMB_SYSINFO,StructTables).Address;
        if (p>=FStructStart) and (ByteValue[p]=SMB_SYSINFO) then begin
          sl:=ByteValue[p+1];
          FSysUUID:='';
          if StructTables[idx].Length>$8 then
            for i:=0 to 15 do
              FSysUUID:=FSysUUID+Format('%2.2x',[ByteValue[p+8+i]]);
          if ByteValue[p+4]>0 then
            FSysMan:=StringValue[p+sl,ByteValue[p+4]];
          if ByteValue[p+5]>0 then
            FSysMod:=StringValue[p+sl,ByteValue[p+5]];
          if ByteValue[p+6]>0 then
            FSysVer:=StringValue[p+sl,ByteValue[p+6]];
          if ByteValue[p+7]>0 then
            FSysSN:=StringValue[p+sl,ByteValue[p+7]];
        end;

        //Table 2
        p:=FindTableRecord(SMB_BASEINFO,StructTables).Address;
        if (p>=FStructStart) and (ByteValue[p]=SMB_BASEINFO) then begin
          sl:=ByteValue[p+1];
          if ByteValue[p+4]>0 then
            FMBMan:=StringValue[p+sl,ByteValue[p+4]];
          if ByteValue[p+5]>0 then
            FMBMod:=StringValue[p+sl,ByteValue[p+5]];
          if ByteValue[p+6]>0 then
            FMBVer:=StringValue[p+sl,ByteValue[p+6]];
          if ByteValue[p+7]>0 then
            FMBSN:=StringValue[p+sl,ByteValue[p+7]];
          if ByteValue[p+8]>0 then
            FMBAT:=StringValue[p+sl,ByteValue[p+8]];
          if FindTableRecord(SMB_BASEINFO,StructTables).Length>=$A then
            if ByteValue[p+$A]>0 then
              FMBLIC:=StringValue[p+sl,ByteValue[p+$A]];
        end;

        //Table 3
        p:=FindTableRecord(SMB_SYSENC,StructTables).Address;
        if (p>=FStructStart) and (ByteValue[p]=SMB_SYSENC) then begin
          sl:=ByteValue[p+1];
          FCHMod:=TSMBIOS_Chassis((ByteValue[p+5] and $7F)-1);
          if ByteValue[p+4]>0 then
            FCHMan:=StringValue[p+sl,ByteValue[p+4]];
          if ByteValue[p+6]>0 then
            FCHVer:=StringValue[p+sl,ByteValue[p+6]];
          if ByteValue[p+7]>0 then
            FCHSN:=StringValue[p+sl,ByteValue[p+7]];
          if ByteValue[p+8]>0 then
            FCHAT:=StringValue[p+sl,ByteValue[p+8]];
        end;

        //Table 4
        idx:=FindTableIndex(SMB_CPU,StructTables);
        if idx>-1 then
          repeat
            p:=StructTables[idx].Address;
            if (p>=FStructStart) and (ByteValue[p]=SMB_CPU) then begin
              sl:=ByteValue[p+1];
              SetLength(FProc,Length(FProc)+1);
              with FProc[High(FProc)] do begin
                Typ:=ByteValue[p+$5];
                Family:=ByteValue[p+$6];
                b:=ByteValue[p+$11];
                if IsBitOn(b,7) then begin
                  Voltage:=(b-$80)/10
                end else begin
                  if IsBitOn(b,0) then
                    Voltage:=5
                  else
                    if IsBitOn(b,1) then
                      Voltage:=3.3
                    else
                      if IsBitOn(b,2) then
                        Voltage:=2.9
                end;
                Upgrade:=TSMBIOS_Upgrade(ByteValue[p+$19]-1);
                w:=Wordvalue[p+$12];
                ExternalClock:=w;
                w:=Wordvalue[p+$16];
                Frequency:=w;
                if ByteValue[p+4]>0 then
                  Socket:=StringValue[p+sl,ByteValue[p+4]];
                if ByteValue[p+7]>0 then
                  Manufacturer:=StringValue[p+sl,ByteValue[p+7]];
                if ByteValue[p+$10]>0 then
                  Version:=StringValue[p+sl,ByteValue[p+$10]];
                SerialNumber:='';
                PartNumber:='';
                AssetTag:='';
                if (FMajorVersion>2) or (FMinorVersion>=3) then begin
                  if ByteValue[p+$20]>0 then
                    SerialNumber:=StringValue[p+sl,ByteValue[p+$20]];
                  if ByteValue[p+$21]>0 then
                    AssetTag:=StringValue[p+sl,ByteValue[p+$21]];
                  if ByteValue[p+$22]>0 then
                    PartNumber:=StringValue[p+sl,ByteValue[p+$22]];
                  if ((MajorVersion=2) and (MinorVersion>=5)) or (MajorVersion>2) then begin
                    CoreCount:=ByteValue[p+$23];
                    CoreEnabled:=ByteValue[p+$24];
                    ThreadCount:=ByteValue[p+$25];
                    Flags:=WordValue[p+$26];
                    Family2:=WordValue[p+$28];
                  end;
                end;
              end;
            end;
            if Trim(FProc[High(FProc)].Version)='' then
              Fproc[High(FProc)].Version:=Format('Processor_%d',[High(FProc)]);
            idx:=FindTableIndex(SMB_CPU,StructTables,idx+1);
          until idx=-1;

        //Table 5
        idx:=FindTableIndex(SMB_MEMCTRL,StructTables);
        if idx>-1 then
          repeat
            p:=StructTables[idx].Address;
            if (p>=FStructStart) and (ByteValue[p]=SMB_MEMCTRL) then begin
              FMCSI:=TSMBIOS_InterleaveSupport(ByteValue[p+$6]-1);
              FMCCI:=TSMBIOS_InterleaveSupport(ByteValue[p+$7]-1);
              try
                FMCMS:=Round(Power(2,ByteValue[p+$8]));
              except
                FMCMS:=0;
              end;
              w:=WordValue[p+$9];
              FMCSS:=[];
              for i:=0 to 4 do
                if IsBitOn(w,i) then
                  FMCSS:=FMCSS+[TSMBIOS_MemorySpeed(i)];
              w:=WordValue[p+$B];
              FMCST:=[];
              for i:=0 to 10 do
                if IsBitOn(w,i) then
                  FMCST:=FMCST+[TSMBIOS_Memorytype(i)];
              b:=ByteValue[p+$D];
              FMCSV:=[];
              for i:=0 to 2 do
                if IsBitOn(b,i) then
                  FMCSV:=FMCSV+[TSMBIOS_Voltage(i)];
              FMCSC:=ByteValue[p+$E];
            end;
            idx:=FindTableIndex(SMB_MEMCTRL,StructTables,idx+1);
          until idx=-1;

        //Table 6
        idx:=FindTableIndex(SMB_MEMMOD,StructTables);
        if idx>-1 then
        repeat
          p:=StructTables[idx].Address;
          if (p>=FStructStart) and (ByteValue[p]=SMB_MEMMOD) then begin
            sl:=ByteValue[p+1];
            SetLength(FMemMod,Length(FMemMod)+1);
            j:=High(FMemMod);
            with FMemMod[j] do begin
              if ByteValue[p+4]>0 then
                Socket:=StringValue[p+sl,ByteValue[p+4]];
              Speed:=ByteValue[p+6];
              w:=WordValue[p+7];
              Types:=[];
              for i:=0 to 10 do
                if IsBitOn(w,i) then
                  Types:=Types+[TSMBIOS_Memorytype(i)];
              b:=ByteValue[p+$9];
              if b>=128 then
                b:=b xor 128;
              if b in [$7D..$7F] then
                Size:=0
              else
                try
                  Size:=Round(Power(2,b));
                except
                  Size:=0;
                end;
            end;
          end;
          idx:=FindTableIndex(SMB_MEMMOD,StructTables,idx+1);
        until idx=-1;

        //Table 7
        idx:=FindTableIndex(SMB_CACHE,StructTables);
        if idx>-1 then
        repeat
          p:=StructTables[idx].Address;
          if (p>=FStructStart) and (ByteValue[p]=SMB_CACHE) then begin
            sl:=ByteValue[p+1];
            SetLength(FCache,Length(FCache)+1);
            with FCache[High(FCache)] do begin
              if ByteValue[p+4]>0 then
                Designation:=StringValue[p+sl,ByteValue[p+4]];
              w:=WordValue[p+$7];
              if w>=32768 then
                w:=w xor 32768;
              if IsBitOn(WordValue[p+$7],15) then
                MaxSize:=w*64
              else
                MaxSize:=w;
              w:=WordValue[p+$9];
              if w>=32768 then
                w:=w xor 32768;
              if IsBitOn(WordValue[p+$9],15) then
                InstalledSize:=w*64
              else
                InstalledSize:=w;
              w:=WordValue[p+$D];
              for i:=0 to 6 do
                if IsBitOn(w,i) then
                  SRAMType:=TSMBIOS_SRAMType(i);
              if StructTables[idx].Length<=$F then begin
                Associativity:=caUnknown;
                Typ:=ctUnknown;
                Speed:=0;
              end else begin
                try
                  if ByteValue[p+$12]-1 in [integer(Low(TSMBIOS_CacheAssociativity))..integer(High(TSMBIOS_CacheAssociativity))] then
                    Associativity:=TSMBIOS_CacheAssociativity(ByteValue[p+$12]-1)
                  else
                    Associativity:=caUnknown;
                except
                  Associativity:=caUnknown;
                end;
                try
                  if ByteValue[p+$11]-1 in [integer(Low(TSMBIOS_CacheType))..integer(High(TSMBIOS_CacheType))] then
                    Typ:=TSMBIOS_CacheType(ByteValue[p+$11]-1)
                  else
                    Typ:=ctUnknown;
                except
                  Typ:=ctUnknown;
                end;
                Speed:=ByteValue[p+$F];
              end;
            end;
            if Trim(FCache[High(FCache)].Designation)='' then
              FCache[High(FCache)].Designation:=Format('Cache_%d',[High(FCache)]);
          end;
          idx:=FindTableIndex(SMB_CACHE,StructTables,idx+1);
        until idx=-1;

        //Table 8
        idx:=FindTableIndex(SMB_PORTCON,StructTables);
        if idx>-1 then
        repeat
          p:=StructTables[idx].Address;
          if (p>=FStructStart) and (ByteValue[p]=SMB_PORTCON) then begin
            sl:=ByteValue[p+1];
            SetLength(FPort,Length(FPort)+1);
            with FPort[High(FPort)] do begin
              if ByteValue[p+4]>0 then
                InternalDesignator:=StringValue[p+sl,ByteValue[p+4]];
              b:=Bytevalue[p+5];
              if b<$FF then begin
                if b>$2F then
                  b:=b-$7D;
                InternalConnector:=TSMBIOS_ConnectorType(b);
              end else
                InternalConnector:=smctOther;
              if ByteValue[p+6]>0 then
                ExternalDesignator:=StringValue[p+sl,ByteValue[p+6]];
              b:=Bytevalue[p+7];
              if b<$FF then begin
                if b>$2F then
                  b:=b-$7D;
                ExternalConnector:=TSMBIOS_ConnectorType(b)
              end else
                ExternalConnector:=smctOther;
              b:=Bytevalue[p+8];
              if b<$FF then begin
                if b>$2F then
                  b:=b-$7E;
                Typ:=TSMBIOS_PortType(b);
              end else
                Typ:=smptOther;
            end;
            if Trim(FPort[High(FPort)].ExternalDesignator)='' then
              FPort[High(FPort)].ExternalDesignator:=Format('PortSlot_%d',[High(FPort)]);
          end;
          idx:=FindTableIndex(SMB_PORTCON,StructTables,idx+1);
        until idx=-1;

        //Table 9
        idx:=FindTableIndex(SMB_SLOTS,StructTables);
        if idx>-1 then
          repeat
            p:=StructTables[idx].Address;
            if (p>=FStructStart) and (ByteValue[p]=SMB_SLOTS) then begin
              sl:=ByteValue[p+1];
              SetLength(FSlot,Length(FSlot)+1);
              with FSlot[High(FSlot)] do begin
                if ByteValue[p+4]>0 then
                  Designation:=StringValue[p+sl,ByteValue[p+4]];
                b:=Bytevalue[p+5];
                if b>$13 then
                  Typ:=TSMBIOS_SlotType(b-$8D)
                else
                  Typ:=TSMBIOS_SlotType(b-1);
                DataBus:=TSMBIOS_DataBusType(Bytevalue[p+6]-1);
                Usage:=TSMBIOS_SlotUsage(Bytevalue[p+7]-1);
                Length:=TSMBIOS_SlotLength(Bytevalue[p+8]-1);
                ID:=WordValue[p+9];
                if (FMajorVersion>2) or (FMinorVersion>=6) then begin
                  SegmentGroup:=WordValue[p+$D];
                  BusNumber:=ByteValue[p+$F];
                  b:=ByteValue[p+$10];
                  DevNumber:=b shr 3;
                  FuncNumber:=b and 7;
                end;
              end;
              if Trim(FSlot[High(FSlot)].Designation)='' then
                FSlot[High(FSlot)].Designation:=Format('Slot_%d',[High(FSlot)]);
            end;
            idx:=FindTableIndex(SMB_SLOTS,StructTables,idx+1);
          until idx=-1;

        //Table 10
        idx:=FindTableIndex(SMB_ONBOARD,StructTables);
        if idx>-1 then
          repeat
            p:=StructTables[idx].Address;
            if (p>=FStructStart) and (ByteValue[p]=SMB_ONBOARD) then begin
              c:=(StructTables[idx].Length-4) div 2;
              sl:=ByteValue[p+1];
              for i:=0 to c-1 do begin
                SetLength(FOBD,Length(FOBD)+1);
                with FOBD[High(FOBD)] do begin
                  b:=ByteValue[p+4+2*((i+1)-1)];
                  if b>128 then
                    b:=b-128;
                  if (b-1)<=Integer(High(TSMBIOS_OnBoardDeviceType)) then
                    Typ:=TSMBIOS_OnBoardDeviceType(b-1)
                  else
                    Typ:=obdOther;
                  b:=ByteValue[p+4+2*((i+1)-1)];
                  Status:=IsBitOn(b,7);
                  if ByteValue[p+5+2*((i+1)-1)]>0 then
                    DeviceName:=StringValue[p+sl,ByteValue[p+5+2*((i+1)-1)]];
                end;
              end;
            end;
            idx:=FindTableIndex(SMB_ONBOARD,StructTables,idx+1);
          until idx=-1;

        //Table 16
        idx:=FindTableIndex(SMB_PHYSMEM,StructTables);
        if idx>-1 then
          repeat
            p:=StructTables[idx].Address;
            if (p>=FStructStart) and (ByteValue[p]=SMB_PHYSMEM) then begin
              FPMALoc:=TSMBIOS_PMALocation(ByteValue[p+$4]-1);
              FPMAUse:=TSMBIOS_PMAUse(ByteValue[p+$5]-1);
              FPMAECT:=TSMBIOS_PMAErrorCorrectionType(ByteValue[p+$6]-1);
              FPMAMC:=DWORDValue[p+$7];
              if (FPMAMC=$80000000) and (((MajorVersion>=2) and (MinorVersion>=7)) or (MajorVersion>2)) then
                FPMAMC:=QWORDValue[p+$F];
              FPMADN:=WORDValue[p+$D];
            end;
            idx:=FindTableIndex(SMB_PHYSMEM,StructTables,idx+1);
          until idx=-1;

        //Table 17
        idx:=FindTableIndex(SMB_MEMDEV,StructTables);
        if idx>-1 then
          repeat
            p:=StructTables[idx].Address;
            if (p>=FStructStart) and (ByteValue[p]=SMB_MEMDEV) then begin
              sl:=ByteValue[p+1];
              SetLength(FMemDev,Length(FMemDev)+1);
              with FMemDev[High(FMemDev)] do begin
                TotalWidth:=WordValue[p+$08];
                DataWidth:=WordValue[p+$0A];
                Size:=WordValue[p+$0C];
                FormFactor:=TSMBIOS_MemoryFormFactor(ByteValue[p+$0E]-1);
                if ByteValue[p+$10]>0 then
                  DeviceLocator:=StripUnprintable(StringValue[p+sl,ByteValue[p+$10]],#0);
                if ByteValue[p+$11]>0 then
                  BankLocator:=StringValue[p+sl,ByteValue[p+$11]];
                b:=ByteValue[p+$12];
                if b>128 then
                  b:=b-128;
                Device:=TSMBIOS_MemoryDeviceType(b-1);
                w:=WordValue[p+$13];
                TypeDetails:=[];
                for i:=0 to 12 do
                  if IsBitOn(w,i) then
                    TypeDetails:=TypeDetails+[TSMBIOS_MemoryTypeDetail(i)];
                Speed:=0;
                Manufacturer:='';
                SerialNumber:='';
                PartNumber:='';
                AssetTag:='';
                if ((MajorVersion>=2) and (MinorVersion>=3)) or (MajorVersion>2) then begin
                  MaxSpeed:=WordValue[p+$15];
                  if ByteValue[p+$17]>0 then
                    Manufacturer:=StringValue[p+sl,ByteValue[p+$17]];
                  if ByteValue[p+$18]>0 then
                    SerialNumber:=StringValue[p+sl,ByteValue[p+$18]];
                  if ByteValue[p+$19]>0 then
                    AssetTag:=StringValue[p+sl,ByteValue[p+$19]];
                  if ByteValue[p+$1A]>0 then
                    PartNumber:=StringValue[p+sl,ByteValue[p+$1A]];
                end;
                if ((MajorVersion>=2) and (MinorVersion>=7)) or (MajorVersion>2) then begin
                  if (Size=$7FFF) then
                    Size:=DWordValue[p+$1C];
                  Speed:=WordValue[p+$20];
                end;
                if ((MajorVersion>=2) and (MinorVersion>=7)) or (MajorVersion>2) then begin
                  MinimumVoltage:=WordValue[p+$22];
                  MaximumVoltage:=WordValue[p+$24];
                  ConfiguredVoltage:=WordValue[p+$26];
                end;
                if ((MajorVersion>=3) and (MinorVersion>=2)) or (MajorVersion>3) then begin
                  MemoryTechnology:=TSMBIOS_MemoryTechnology(ByteValue[p+$28]);
                  OperatingModeCapability:=WordValue[p+$29];
                  FirmwareVersion:=ByteValue[p+$2B];
                  ModuleManufacturerID:=WordValue[p+$2C];
                  ModuleProductID:=WordValue[p+$2E];
                  SubsystemControllerManufacturerID:=WordValue[p+$30];
                  SubsystemControllerProductID:=WordValue[p+$32];
                  NonVolatileSize:=QWordValue[p+$34];
                  VolatileSize:=QWordValue[p+$3C];
                  CacheSize:=QWordValue[p+$44];
                  LogicalSize:=WordValue[p+$4C];
                end;
              end;
            end;
            idx:=FindTableIndex(SMB_MEMDEV,StructTables,idx+1);
          until idx=-1;

        //Table 22
        idx:=FindTableIndex(SMB_BATTERY,StructTables);
        if idx>-1 then
          repeat
            p:=StructTables[idx].Address;
            if (p>=FStructStart) and (ByteValue[p]=SMB_BATTERY) then begin
              sl:=ByteValue[p+1];
              SetLength(FBat,Length(FBat)+1);
              with FBat[High(FBat)] do begin
                if ByteValue[p+$4]>0 then
                  Location:=Trim(StringValue[p+sl,ByteValue[p+$4]]);
                if ByteValue[p+$5]>0 then
                  Manufacturer:=Trim(StringValue[p+sl,ByteValue[p+$5]]);
                if ByteValue[p+$6]>0 then
                  ManufacturerDate:=Trim(StringValue[p+sl,ByteValue[p+$6]]);
                if ByteValue[p+$7]>0 then
                  SerialNumber:=Trim(StringValue[p+sl,ByteValue[p+$7]]);
                if ByteValue[p+$8]>0 then
                  DeviceName:=Trim(StringValue[p+sl,ByteValue[p+$8]]);
                DeviceChemistry:=TSMBIOS_DeviceChemistry(ByteValue[p+$9]-1);
                DesignCapacity:=WordValue[p+$A];;
                DesignVoltage:=WordValue[p+$C];
                if ByteValue[p+$E]>0 then
                  SBDSVersionNumber:=Trim(StringValue[p+sl,ByteValue[p+$E]]);
                MaxErrorInBatData:=WordValue[p+$F];

                SBDSSerialNumber:=WordValue[p+$10];
                SBDSManufactureDate:=WordValue[p+$12];
                if ByteValue[p+$14]>0 then
                  SBDSDeviceChemistry:=Trim(StringValue[p+sl,ByteValue[p+$14]]);
                DesignCapacityMultiplier:=ByteValue[p+$15];
                OEMSpec:=DWordValue[p+$16];
              end;
            end;
            idx:=FindTableIndex(SMB_BATTERY,StructTables,idx+1);
          until idx=-1;

        //Table 26
        idx:=FindTableIndex(SMB_VOLTAGE,StructTables);
        if idx>-1 then
          repeat
            p:=StructTables[idx].Address;
            if (p>=FStructStart) and (ByteValue[p]=SMB_VOLTAGE) then begin
              sl:=ByteValue[p+1];
              SetLength(FVP,Length(FVP)+1);
              with FVP[High(FVP)] do begin
                if ByteValue[p+$4]>0 then
                  Description:=Trim(StringValue[p+sl,ByteValue[p+$4]]);
                b:=Bytevalue[p+$5];
                s:=Copy(ByteToBin(b),1,3);
                if s='001' then
                  Status:=TSMBIOS_StatusType(0)
                else if s='010' then
                  Status:=TSMBIOS_StatusType(1)
                else if s='011' then
                  Status:=TSMBIOS_StatusType(2)
                else if s='100' then
                  Status:=TSMBIOS_StatusType(3)
                else if s='101' then
                  Status:=TSMBIOS_StatusType(4)
                else if s='110' then
                  Status:=TSMBIOS_StatusType(5)
                else
                  Status:=TSMBIOS_StatusType(1);
                s:=Copy(ByteToBin(b),4,8);
                if s='00001' then
                  Location:=TSMBIOS_VoltProbeLocationType(0)
                else if s='00010' then
                  Location:=TSMBIOS_VoltProbeLocationType(1)
                else if s='00011' then
                  Location:=TSMBIOS_VoltProbeLocationType(2)
                else if s='00100' then
                  Location:=TSMBIOS_VoltProbeLocationType(3)
                else if s='00101' then
                  Location:=TSMBIOS_VoltProbeLocationType(4)
                else if s='00110' then
                  Location:=TSMBIOS_VoltProbeLocationType(5)
                else if s='00111' then
                  Location:=TSMBIOS_VoltProbeLocationType(6)
                else if s='01000' then
                  Location:=TSMBIOS_VoltProbeLocationType(7)
                else if s='01001' then
                  Location:=TSMBIOS_VoltProbeLocationType(8)
                else if s='01010' then
                  Location:=TSMBIOS_VoltProbeLocationType(9)
                else if s='01011' then
                  Location:=TSMBIOS_VoltProbeLocationType(10)
                else
                  Location:=TSMBIOS_VoltProbeLocationType(0);

                Max:=WordValue[p+$6];
                Min:=WordValue[p+$8];
                Resolution:=WordValue[p+$A];
                Tolerance:=WordValue[p+$C];
                Accuracy:=WordValue[p+$E];
                if StructTables[idx].Length>$14 then
                  NominalValue:=Wordvalue[p+$14]
                else
                  NominalValue:=0;
              end;
            end;
            idx:=FindTableIndex(SMB_VOLTAGE,StructTables,idx+1);
          until idx=-1;

        //Table 27
        idx:=FindTableIndex(SMB_COOL,StructTables);
        if idx>-1 then
          repeat
            p:=StructTables[idx].Address;
            if (p>=FStructStart) and (ByteValue[p]=SMB_COOL) then begin
              sl:=ByteValue[p+1];
              SetLength(FCD,Length(FCD)+1);
              with FCD[High(FCD)] do begin
                if ByteValue[p+$0E]>0 then
                  Description:=Trim(StringValue[p+sl,ByteValue[p+$0E]]);
                b:=Bytevalue[p+$6];
                s:=Copy(ByteToBin(b),1,3);
                if s='001' then
                  Status:=TSMBIOS_StatusType(0)
                else if s='010' then
                  Status:=TSMBIOS_StatusType(1)
                else if s='011' then
                  Status:=TSMBIOS_StatusType(2)
                else if s='100' then
                  Status:=TSMBIOS_StatusType(3)
                else if s='101' then
                  Status:=TSMBIOS_StatusType(4)
                else if s='110' then
                  Status:=TSMBIOS_StatusType(5)
                else
                  Status:=TSMBIOS_StatusType(1);
                s:=Copy(ByteToBin(b),4,8);
                if s='00001' then
                  Typ:=TSMBIOS_CoolingType(0)
                else if s='00010' then
                  Typ:=TSMBIOS_CoolingType(1)
                else if s='00011' then
                  Typ:=TSMBIOS_CoolingType(2)
                else if s='00100' then
                  Typ:=TSMBIOS_CoolingType(3)
                else if s='00101' then
                  Typ:=TSMBIOS_CoolingType(4)
                else if s='00110' then
                  Typ:=TSMBIOS_CoolingType(5)
                else if s='00111' then
                  Typ:=TSMBIOS_CoolingType(6)
                else if s='01000' then
                  Typ:=TSMBIOS_CoolingType(7)
                else if s='01001' then
                  Typ:=TSMBIOS_CoolingType(8)
                else if s='01010' then
                  Typ:=TSMBIOS_CoolingType(9)
                else if s='01011' then
                  Typ:=TSMBIOS_CoolingType(10)
                else
                  Typ:=TSMBIOS_CoolingType(0);
                TemperatureProbeHandle:=WordValue[p+$4];
                GroupUnit:=ByteValue[p+$7];
                NominalSpeed:=WordValue[p+$C];
              end;
            end;
            idx:=FindTableIndex(SMB_COOL,StructTables,idx+1);
          until idx=-1;

        //Table 28
        idx:=FindTableIndex(SMB_TEMP,StructTables);
        if idx>-1 then
          repeat
            p:=StructTables[idx].Address;
            if (p>=FStructStart) and (ByteValue[p]=SMB_TEMP) then begin
              sl:=ByteValue[p+1];
              SetLength(FTP,Length(FTP)+1);
              with FTP[High(FTP)] do begin
                if ByteValue[p+$4]>0 then
                  Description:=Trim(StringValue[p+sl,ByteValue[p+$4]]);
                b:=Bytevalue[p+$5];
                s:=Copy(ByteToBin(b),1,3);
                if s='001' then
                  Status:=TSMBIOS_StatusType(0)
                else if s='010' then
                  Status:=TSMBIOS_StatusType(1)
                else if s='011' then
                  Status:=TSMBIOS_StatusType(2)
                else if s='100' then
                  Status:=TSMBIOS_StatusType(3)
                else if s='101' then
                  Status:=TSMBIOS_StatusType(4)
                else if s='110' then
                  Status:=TSMBIOS_StatusType(5)
                else
                  Status:=TSMBIOS_StatusType(1);
                s:=Copy(ByteToBin(b),4,8);
                if s='00001' then
                  Location:=TSMBIOS_TempProbeLocationType(0)
                else if s='00010' then
                  Location:=TSMBIOS_TempProbeLocationType(1)
                else if s='00011' then
                  Location:=TSMBIOS_TempProbeLocationType(2)
                else if s='00100' then
                  Location:=TSMBIOS_TempProbeLocationType(3)
                else if s='00101' then
                  Location:=TSMBIOS_TempProbeLocationType(4)
                else if s='00110' then
                  Location:=TSMBIOS_TempProbeLocationType(5)
                else if s='00111' then
                  Location:=TSMBIOS_TempProbeLocationType(6)
                else if s='01000' then
                  Location:=TSMBIOS_TempProbeLocationType(7)
                else if s='01001' then
                  Location:=TSMBIOS_TempProbeLocationType(8)
                else if s='01010' then
                  Location:=TSMBIOS_TempProbeLocationType(9)
                else if s='01011' then
                  Location:=TSMBIOS_TempProbeLocationType(10)
                else
                  Location:=TSMBIOS_TempProbeLocationType(0);
                Max:=WordValue[p+$6];
                Min:=WordValue[p+$8];
                Resolution:=WordValue[p+$A];
                Tolerance:=WordValue[p+$C];
                Accuracy:=WordValue[p+$E];
                if StructTables[idx].Length>$14 then
                  NominalValue:=Wordvalue[p+$14]
                else
                  NominalValue:=0;
              end;
            end;
            idx:=FindTableIndex(SMB_TEMP,StructTables,idx+1);
          until idx=-1;

        //Table 29
        idx:=FindTableIndex(SMB_CURRENT,StructTables);
        if idx>-1 then
          repeat
            p:=StructTables[idx].Address;
            if (p>=FStructStart) and (ByteValue[p]=SMB_CURRENT) then begin
              sl:=ByteValue[p+1];
              SetLength(FCP,Length(FCP)+1);
              with FCP[High(FCP)] do begin
                if ByteValue[p+$4]>0 then
                  Description:=Trim(StringValue[p+sl,ByteValue[p+$4]]);
                b:=Bytevalue[p+$5];
                s:=Copy(ByteToBin(b),1,3);
                if s='001' then
                  Status:=TSMBIOS_StatusType(0)
                else if s='010' then
                  Status:=TSMBIOS_StatusType(1)
                else if s='011' then
                  Status:=TSMBIOS_StatusType(2)
                else if s='100' then
                  Status:=TSMBIOS_StatusType(3)
                else if s='101' then
                  Status:=TSMBIOS_StatusType(4)
                else if s='110' then
                  Status:=TSMBIOS_StatusType(5)
                else
                  Status:=TSMBIOS_StatusType(1);
                s:=Copy(ByteToBin(b),4,8);
                if s='00001' then
                  Location:=TSMBIOS_CurrProbeLocationType(0)
                else if s='00010' then
                  Location:=TSMBIOS_CurrProbeLocationType(1)
                else if s='00011' then
                  Location:=TSMBIOS_CurrProbeLocationType(2)
                else if s='00100' then
                  Location:=TSMBIOS_CurrProbeLocationType(3)
                else if s='00101' then
                  Location:=TSMBIOS_CurrProbeLocationType(4)
                else if s='00110' then
                  Location:=TSMBIOS_CurrProbeLocationType(5)
                else if s='00111' then
                  Location:=TSMBIOS_CurrProbeLocationType(6)
                else if s='01000' then
                  Location:=TSMBIOS_CurrProbeLocationType(7)
                else if s='01001' then
                  Location:=TSMBIOS_CurrProbeLocationType(8)
                else if s='01010' then
                  Location:=TSMBIOS_CurrProbeLocationType(9)
                else if s='01011' then
                  Location:=TSMBIOS_CurrProbeLocationType(10)
                else
                  Location:=TSMBIOS_CurrProbeLocationType(0);
                Max:=WordValue[p+$6];
                Min:=WordValue[p+$6];
                Resolution:=WordValue[p+$A];
                Tolerance:=WordValue[p+$C];
                Accuracy:=WordValue[p+$E];
                if StructTables[idx].Length>$14 then
                  NominalValue:=Wordvalue[p+$14]
                else
                  NominalValue:=0;
              end;
            end;
            idx:=FindTableIndex(SMB_CURRENT,StructTables,idx+1);
          until idx=-1;

        //Table 39
        idx:=FindTableIndex(SMB_SPS,StructTables);
        if idx>-1 then
          repeat
            p:=StructTables[idx].Address;
            if (p>=FStructStart) and (ByteValue[p]=SMB_SPS) then begin
              sl:=ByteValue[p+1];
              FSPSPUG:=ByteValue[p+$4];
              if ByteValue[p+$5]>0 then
                FSPSLoc:=Trim(StringValue[p+sl,ByteValue[p+$5]]);
              if ByteValue[p+$6]>0 then
                FSPSDN:=Trim(StringValue[p+sl,ByteValue[p+$6]]);
              if ByteValue[p+$7]>0 then
                FSPSM:=Trim(StringValue[p+sl,ByteValue[p+$7]]);
              if ByteValue[p+$8]>0 then
                FSPSSN:=Trim(StringValue[p+sl,ByteValue[p+$8]]);
              if ByteValue[p+$9]>0 then
                FSPSATN:=Trim(StringValue[p+sl,ByteValue[p+$9]]);
              if ByteValue[p+$A]>0 then
                FSPSMPN:=Trim(StringValue[p+sl,ByteValue[p+$A]]);
              if ByteValue[p+$B]>0 then
                FSPSRL:=Trim(StringValue[p+sl,ByteValue[p+$B]]);
              if ByteValue[p+$7]>0 then
                FSPSM:=Trim(StringValue[p+sl,ByteValue[p+$7]]);
              FSPSMPC:=WordValue[p+$C];
              FSPSC:=WordValue[p+$E];
              FSPSIVPH:=WordValue[p+$10];
              FSPSCDH:=WordValue[p+$12];
              FSPSICPH:=WordValue[p+$14];
            end;
            idx:=FindTableIndex(SMB_SPS,StructTables,idx+1);
          until idx=-1;

        //Table 41
        idx:=FindTableIndex(SMB_ONBOARDX,StructTables);
        if idx>-1 then
          repeat
            p:=StructTables[idx].Address;
            if (p>=FStructStart) and (ByteValue[p]=SMB_ONBOARDX) then begin
              sl:=ByteValue[p+1];
              SetLength(FOBDX,Length(FOBDX)+1);
              with FOBDX[High(FOBDX)] do begin
                if ByteValue[p+$4]>0 then
                  DeviceName:=Trim(StringValue[p+sl,ByteValue[p+$4]]);
                b:=Bytevalue[p+$5];
                if ((b and 127)-1)<=integer(High(TSMBIOS_OnBoardDeviceType)) then
                  Typ:=TSMBIOS_OnBoardDeviceType((b and 127)-1)
                else
                  Typ:=obdUnknown;
                Status:=IsBitOn(b,7);
                Instance:=ByteValue[p+$6];
                SegmentGroupNumber:=WordValue[p+$7];
                BusNumber:=ByteValue[p+$9];
                b:=ByteValue[p+$A];
                DeviceNumber:=BinToInt(Copy(ByteToBin(b),1,4));
                FunctionNumber:=BinToInt(Copy(ByteToBin(b),5,3));
              end;
            end;
            idx:=FindTableIndex(SMB_ONBOARDX,StructTables,idx+1);
          until idx=-1;

        //Table 43
        idx:=FindTableIndex(SMB_TPMDEV,StructTables);
        if idx>-1 then
          repeat
            p:=StructTables[idx].Address;
            if (p>=FStructStart) and (ByteValue[p]=SMB_TPMDEV) then begin
              sl:=ByteValue[p+1];
              SetLength(FTPM,Length(FTPM)+1);
              with FTPM[High(FTPM)] do begin
                if ByteValue[p+$12]>0 then
                  Description:=Trim(StringValue[p+sl,ByteValue[p+$12]]);
                VendorID:=Chr(Bytevalue[p+$4])+Chr(Bytevalue[p+$5])+Chr(Bytevalue[p+$6])+Chr(Bytevalue[p+$7]);
                MajorSpecVersion:=ByteValue[p+$8];
                MinorSpecVersion:=ByteValue[p+$9];
                FirmwareVersion1:=DWordValue[p+$A];
                FirmwareVersion2:=DWordValue[p+$E];
                Chars:=QWordValue[p+$13];
                OemDef:=DWordValue[p+$1B];
              end;
            end;
            idx:=FindTableIndex(SMB_TPMDEV,StructTables,idx+1);
          until idx=-1;

        if (SameText(FSysMan,'HP') or SameText(FSysMan,'HPE') or SameText(FSysMan,'Hewlett-Packard') or SameText(FSysMan,'Hewlett Packard Enterprise')) then begin
          //Table 204 - HP ProLiant System/Rack Locator
          idx:=FindTableIndex(SMB_HP_204,StructTables);
          if idx>-1 then
            repeat
              p:=StructTables[idx].Address;
              if (p>=FStructStart) and (ByteValue[p]=SMB_HP_204) then begin
                sl:=ByteValue[p+1];
                if ByteValue[p+$4]>0 then
                  FHP204.RackName:=Trim(StringValue[p+sl,ByteValue[p+$4]]);
                if ByteValue[p+$5]>0 then
                  FHP204.EnclosureName:=Trim(StringValue[p+sl,ByteValue[p+$5]]);
                if ByteValue[p+$6]>0 then
                  FHP204.EnclosureModel:=Trim(StringValue[p+sl,ByteValue[p+$6]]);
                if ByteValue[p+$7]>0 then
                  FHP204.ServerBay:=Trim(StringValue[p+sl,ByteValue[p+$7]]);
                FHP204.EnclosureBays:=ByteValue[p+$8];
                FHP204.BaysFilled:=ByteValue[p+$9];
                if ByteValue[p+$A]>0 then
                  FHP204.EnclosureSerial:=Trim(StringValue[p+sl,ByteValue[p+$A]]);
              end;
              idx:=FindTableIndex(SMB_HP_204,StructTables,idx+1);
            until idx=-1;

          //Table 209 - HP BIOS PXE NIC PCI and MAC Information
          idx:=FindTableIndex(SMB_HP_209,StructTables);
          if idx>-1 then
            repeat
              p:=StructTables[idx].Address;
              if (p>=FStructStart) and (ByteValue[p]=SMB_HP_209) then begin
                SetLength(FHP209,Length(FHP209)+1);
                with FHP209[High(FHP209)] do begin
                  DevNo:=ByteValue[p+$4];
                  BusNo:=ByteValue[p+$5];
                  MAC:=Format('%2.2x:%2.2x:%2.2x:%2.2x:%2.2x:%2.2x',[ByteValue[p+$6],ByteValue[p+$7],ByteValue[p+$8],ByteValue[p+$9],ByteValue[p+$A],ByteValue[p+$B]]);
                end;
                p:=p+$C-$4;
                SetLength(FHP209,Length(FHP209)+1);
                with FHP209[High(FHP209)] do begin
                  DevNo:=ByteValue[p+$4];
                  BusNo:=ByteValue[p+$5];
                  MAC:=Format('%2.2x:%2.2x:%2.2x:%2.2x:%2.2x:%2.2x',[ByteValue[p+$6],ByteValue[p+$7],ByteValue[p+$8],ByteValue[p+$9],ByteValue[p+$A],ByteValue[p+$B]]);
                end;
              end;
              idx:=FindTableIndex(SMB_HP_209,StructTables,idx+1);
            until idx=-1;

          //Table 221 - HP BIOS iSCSI NIC PCI and MAC Information
          idx:=FindTableIndex(SMB_HP_221,StructTables);
          if idx>-1 then
            repeat
              p:=StructTables[idx].Address;
              if (p>=FStructStart) and (ByteValue[p]=SMB_HP_221) then begin
                SetLength(FHP221,Length(FHP221)+1);
                with FHP221[High(FHP221)] do begin
                  DevNo:=ByteValue[p+$4];
                  BusNo:=ByteValue[p+$5];
                  MAC:=Format('%2.2x:%2.2x:%2.2x:%2.2x:%2.2x:%2.2x',[ByteValue[p+$6],ByteValue[p+$7],ByteValue[p+$8],ByteValue[p+$9],ByteValue[p+$A],ByteValue[p+$B]]);
                end;
                p:=p+$C-$4;
                SetLength(FHP221,Length(FHP221)+1);
                with FHP221[High(FHP221)] do begin
                  DevNo:=ByteValue[p+$4];
                  BusNo:=ByteValue[p+$5];
                  MAC:=Format('%2.2x:%2.2x:%2.2x:%2.2x:%2.2x:%2.2x',[ByteValue[p+$6],ByteValue[p+$7],ByteValue[p+$8],ByteValue[p+$9],ByteValue[p+$A],ByteValue[p+$B]]);
                end;
              end;
              idx:=FindTableIndex(SMB_HP_221,StructTables,idx+1);
            until idx=-1;
        end;
      end;
      Finalize(StructTables);
      ScanTables;
    end;
  finally

  end;

  SetDataAvail(True);
end;

function TMiTeC_SMBIOS.GetMemoryModule(Index: Byte): TSMBIOS_MemoryModule;
begin
  Finalize(Result);
  ResetMemory(Result,SizeOf(Result));
  try
    Result:=FMemMod[Index];
  except
  end;
end;

function TMiTeC_SMBIOS.GetMemDevCount: Byte;
begin
  Result:=Length(FMemDev);
end;

function TMiTeC_SMBIOS.GetPort(Index: Byte): TSMBIOS_Port;
begin
  Finalize(Result);
  ResetMemory(Result,SizeOf(Result));
  try
    Result:=FPort[Index];
  except
  end;
end;

function TMiTeC_SMBIOS.GetPortCount: Byte;
begin
  Result:=Length(FPort);
end;

function TMiTeC_SMBIOS.GetSlot(Index: Byte): TSMBIOS_Slot;
begin
  Finalize(Result);
  ResetMemory(Result,SizeOf(Result));
  try
    Result:=FSlot[Index];
  except
  end;
end;

function TMiTeC_SMBIOS.GetSlotCount: Byte;
begin
  Result:=Length(FSlot);
end;

function TMiTeC_SMBIOS.GetBat(Index: Byte): TSMBIOS_Battery;
begin
  Finalize(Result);
  ResetMemory(Result,SizeOf(Result));
  try
    Result:=FBat[Index];
  except
  end;
end;

function TMiTeC_SMBIOS.GetBatCount: Byte;
begin
  Result:=Length(FBat);
end;

function TMiTeC_SMBIOS.GetCache(Index: Byte): TSMBIOS_Cache;
begin
  Finalize(Result);
  ResetMemory(Result,SizeOf(Result));
  try
    Result:=FCache[Index];
  except
  end;
end;

function TMiTeC_SMBIOS.GetCacheCount: Byte;
begin
  Result:=Length(FCache);
end;

function TMiTeC_SMBIOS.GetCoolDev(Index: Byte): TSMBIOS_CoolingDevice;
begin
  Finalize(Result);
  ResetMemory(Result,SizeOf(Result));
  try
    Result:=FCD[Index];
  except
  end;
end;

function TMiTeC_SMBIOS.GetCoolDevCount: Byte;
begin
  Result:=Length(FCD);
end;

function TMiTeC_SMBIOS.GetCurrProbe(Index: Byte): TSMBIOS_CurrentProbe;
begin
  Finalize(Result);
  ResetMemory(Result,SizeOf(Result));
  try
    Result:=FCP[Index];
  except
  end;
end;

function TMiTeC_SMBIOS.GetCurrProbeCount: Byte;
begin
  Result:=Length(FCP);
end;

function TMiTeC_SMBIOS.GetHP209(Index: Byte): TSMBIOS_HP_221;
begin
  Finalize(Result);
  ResetMemory(Result,SizeOf(Result));
  try
    Result:=FHP209[Index];
  except
  end;
end;

function TMiTeC_SMBIOS.GetHP209Count: Byte;
begin
  Result:=Length(FHP209);
end;

function TMiTeC_SMBIOS.GetHP221(Index: Byte): TSMBIOS_HP_221;
begin
  Finalize(Result);
  ResetMemory(Result,SizeOf(Result));
  try
    Result:=FHP221[Index];
  except
  end;
end;

function TMiTeC_SMBIOS.GetHP221Count: Byte;
begin
  Result:=Length(FHP221);
end;

destructor TMiTeC_SMBIOS.Destroy;
begin
  Clear;
  FSMBIOS.Free;
  FStructure.Free;
  inherited;
end;

function TMiTeC_SMBIOS.GetProc(Index: Byte): TSMBIOS_Processor;
begin
  Finalize(Result);
  ResetMemory(Result,SizeOf(Result));
  try
    Result:=FProc[Index];
  except
  end;
end;

function TMiTeC_SMBIOS.GetProcCount: Byte;
begin
  Result:=Length(FProc);
end;

procedure TMiTeC_SMBIOS.Clear;
begin
  Finalize(StructTables);
  Finalize(FMemMod);
  Finalize(FMemdev);
  Finalize(FPort);
  Finalize(FSlot);
  Finalize(FCache);
  Finalize(FProc);
  Finalize(FOBD);
  Finalize(FOBDX);
  Finalize(FTP);
  Finalize(FCD);
  Finalize(FVP);
  Finalize(FCP);
  Finalize(FTPM);
  Finalize(FBat);

  FStart:=0;
  FStructStart:=0;
  FMBMod:='';
  FSysMan:='';
  FSysMod:='';
  FBIOSdate:='';
  FBIOSVendor:='';
  FBIOSVersion:='';
  FBIOSSize:=0;
  FBIOSChars:=[];
  FSysVer:='';
  FSysSN:='';
  FMBSN:='';
  FMBMan:='';
  FMBVer:='';
  FMBAT:='';
  FMBLIC:='';
  FCHSN:='';
  FCHVer:='';
  FCHMan:='';
  FCHAT:='';
  FCHMod:=smchUnknown;
  FMCCI:=smisUnknown;
  FMCSI:=smisUnknown;
  FMCSS:=[];
  FMCST:=[];
  FMCSV:=[];
  FMCMS:=0;
  FMCSC:=0;
  FSysUUID:='';
  FLen:=0;
  FTableCount:=0;

  FSPSMPN:='';
  FSPSIVPH:=0;
  FSPSPUG:=0;
  FSPSSN:='';
  FSPSLoc:='';
  FSPSCDH:=0;
  FSPSC:=0;
  FSPSRL:='';
  FSPSM:='';
  FSPSICPH:=0;
  FSPSATN:='';
  FSPSMPC:=0;
  FSPSDN:='';

  FPMALoc:=TSMBIOS_PMALocation(1);
  FPMAUse:=TSMBIOS_PMAUse(1);
  FPMAECT:=TSMBIOS_PMAErrorCorrectionType(1);
  FPMAMC:=0;
  FPMADN:=0;

  FMajorVersion:=0;
  FMinorVersion:=0;
  FRevisionMajor:=0;
  FRevisionMinor:=0;
end;

constructor TMiTeC_SMBIOS.Create;
begin
  inherited Create(AOwner);
  FNoWMI:=False;
  FLocal:=True;
  FSMBIOS:=TMiTeC_DMA.Create(Self);
  FStructure:=TMiTeC_DMA.Create(Self);
end;

function TMiTeC_SMBIOS.LoadFromStorage;
var
  stg: IStorage;
  SS, Sub, Sub1: TStructuredStorage;

function ReadProcessorFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
  try strm:=Sub1.OpenStream(Format(strm_Proc,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(Self.FProc,Length(Self.FProc)+1);
        with Self.FProc[High(Self.FProc)] do begin
          Manufacturer:=ReadStrProperty(sl,'Manufacturer');
          Version:=ReadStrProperty(sl,'Version');
          Socket:=ReadStrProperty(sl,'Socket');
          SerialNumber:=ReadStrProperty(sl,'SerialNumber');
          AssetTag:=ReadStrProperty(sl,'AssetTag');
          PartNumber:=ReadStrProperty(sl,'PartNumber');
          Frequency:=ReadIntProperty(sl,'Frequency');
          ExternalClock:=ReadIntProperty(sl,'ExternalClock');
          Upgrade:=TSMBIOS_Upgrade(ReadIntProperty(sl,'Upgrade'));
          Voltage:=ReadDblProperty(sl,'Voltage');
          CoreCount:=ReadIntProperty(sl,'CoreCount');
          CoreEnabled:=ReadIntProperty(sl,'CoreEnabled');
          ThreadCount:=ReadIntProperty(sl,'ThreadCount');
          Typ:=ReadIntProperty(sl,'Typ');
          Family:=ReadIntProperty(sl,'Family');
          Family2:=ReadIntProperty(sl,'Family2');
        end;
        Result:=True;
      finally
        sl.Free;
      end;
    finally
      strm.Free;
    end;
end;

function ReadMemModFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
  n: integer;
  mt: TSMBIOS_MemoryType;
begin
  Result:=False;
  try strm:=Sub1.OpenStream(Format(strm_MemMod,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(Self.FMemMod,Length(Self.FMemMod)+1);
        with Self.FMemMod[High(Self.FMemMod)] do begin
          Socket:=ReadStrProperty(sl,'Socket');
          Speed:=ReadIntProperty(sl,'Speed');
          Size:=ReadIntProperty(sl,'Size');
          n:=ReadIntProperty(sl,'Types');
          Types:=[];
          for mt:=Low(TSMBIOS_MemoryType) to High(TSMBIOS_MemoryType) do
            if n and Round(Power(2,Integer(mt)))<>0 then
              Types:=Types+[mt];
        end;
        Result:=True;
      finally
        sl.Free;
      end;
    finally
      strm.Free;
    end;
end;

function ReadMemDevFromStream(AIndex: Integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
  n: integer;
  td: TSMBIOS_MemoryTypeDetail;
begin
  Result:=False;
  try strm:=Sub1.OpenStream(Format(strm_MemDev,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(Self.FMemDev,Length(Self.FMemDev)+1);
        with Self.FMemDev[High(Self.FMemDev)] do begin
          DeviceLocator:=ReadStrProperty(sl,'DeviceLocator');
          BankLocator:=ReadStrProperty(sl,'BankLocator');
          Manufacturer:=ReadStrProperty(sl,'Manufacturer');
          SerialNumber:=ReadStrProperty(sl,'SerialNumber');
          AssetTag:=ReadStrProperty(sl,'AssetTag');
          PartNumber:=ReadStrProperty(sl,'PartNumber');
          Speed:=ReadIntProperty(sl,'Speed');
          Size:=ReadIntProperty(sl,'Size');
          TotalWidth:=ReadIntProperty(sl,'TotalWidth');
          DataWidth:=ReadIntProperty(sl,'DataWidth');
          FormFactor:=TSMBIOS_MemoryFormFactor(ReadIntProperty(sl,'FormFactor'));
          Device:=TSMBIOS_MemoryDeviceType(ReadIntProperty(sl,'Device'));
          n:=ReadIntProperty(sl,'TypeDetails');
          TypeDetails:=[];
          for td:=Low(TSMBIOS_MemoryTypeDetail) to High(TSMBIOS_MemoryTypeDetail) do
            if n and Round(Power(2,Integer(td)))<>0 then
              TypeDetails:=TypeDetails+[td];
          try
            MinimumVoltage:=ReadIntProperty(sl,'MinimumVoltage');
            MaximumVoltage:=ReadIntProperty(sl,'MaximumVoltage');
            ConfiguredVoltage:=ReadIntProperty(sl,'ConfiguredVoltage');
            MemoryTechnology:=TSMBIOS_MemoryTechnology(ReadIntProperty(sl,'MemoryTechnology'));
            OperatingModeCapability:=ReadIntProperty(sl,'OperatingModeCapability');
            FirmwareVersion:=ReadIntProperty(sl,'FirmwareVersion');
            ModuleManufacturerID:=ReadIntProperty(sl,'ModuleManufacturerID');
            ModuleProductID:=ReadIntProperty(sl,'ModuleProductID');
            SubsystemControllerManufacturerID:=ReadIntProperty(sl,'SubsystemControllerManufacturerID');
            SubsystemControllerProductID:=ReadIntProperty(sl,'SubsystemControllerProductID');
            NonVolatileSize:=ReadIntProperty(sl,'NonVolatileSize');
            VolatileSize:=ReadIntProperty(sl,'VolatileSize');
            CacheSize:=ReadIntProperty(sl,'CacheSize');
            LogicalSize:=ReadIntProperty(sl,'LogicalSize');
          except
          end;
        end;
        Result:=True;
        finally
          sl.Free;
        end;
     finally
       strm.Free;
     end;
end;

function ReadPortFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
  try strm:=Sub1.OpenStream(Format(strm_Port,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(Self.FPort,Length(Self.FPort)+1);
        with Self.FPort[High(Self.FPort)] do begin
          InternalDesignator:=ReadStrProperty(sl,'InternalDesignator');
          ExternalDesignator:=ReadStrProperty(sl,'ExternalDesignator');
          InternalConnector:=TSMBIOS_ConnectorType(ReadIntProperty(sl,'InternalConnector'));
          ExternalConnector:=TSMBIOS_ConnectorType(ReadIntProperty(sl,'ExternalConnector'));
          Typ:=TSMBIOS_PortType(ReadIntProperty(sl,'Typ'));
        end;
        Result:=True;
        finally
          sl.Free;
        end;
    finally
      strm.Free;
    end;
end;

function ReadSystemSlotFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
  try strm:=Sub1.OpenStream(Format(strm_Slot,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(Self.FSlot,Length(Self.FSlot)+1);
        with Self.FSlot[High(Self.FSlot)] do begin
          Designation:=ReadStrProperty(sl,'Designation');
          Typ:=TSMBIOS_SlotType(ReadIntProperty(sl,'Typ'));
          DataBus:=TSMBIOS_DataBusType(ReadIntProperty(sl,'DataBus'));
          ID:=ReadIntProperty(sl,'ID');
          Usage:=TSMBIOS_SlotUsage(ReadIntProperty(sl,'Usage'));
          Length:=TSMBIOS_SlotLength(ReadIntProperty(sl,'Length'));
          BusNumber:=ReadIntProperty(sl,'BusNumber');
          DevNumber:=ReadIntProperty(sl,'DevNumber');
          FuncNumber:=ReadIntProperty(sl,'FuncNumber');
        end;
        Result:=True;
      finally
       sl.Free;
      end;
    finally
      strm.Free;
    end;
end;

function ReadCacheFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
  try strm:=Sub1.OpenStream(Format(strm_Cache,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(Self.FCache,Length(Self.FCache)+1);
        with Self.FCache[High(Self.FCache)] do begin
          Designation:=ReadStrProperty(sl,'Designation');
          Typ:=TSMBIOS_CacheType(ReadIntProperty(sl,'Typ'));
          SRAMType:=TSMBIOS_SRAMType(ReadIntProperty(sl,'SRAMType'));
          MaxSize:=ReadIntProperty(sl,'MaxSize');
          InstalledSize:=ReadIntProperty(sl,'InstalledSize');
          Speed:=ReadIntProperty(sl,'Speed');
          Associativity:=TSMBIOS_CacheAssociativity(ReadIntProperty(sl,'Associativity'));
        end;
        Result:=True;
      finally
        sl.Free;
      end;
    finally
      strm.Free;
    end;
end;

function ReadOBDFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
  try strm:=Sub1.OpenStream(Format(strm_OBD,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(Self.FOBD,Length(Self.FOBD)+1);
        with Self.FOBD[High(Self.FOBD)] do begin
          DeviceName:=ReadStrProperty(sl,'DeviceName');
          Typ:=TSMBIOS_OnBoardDeviceType(ReadIntProperty(sl,'Typ'));
          Status:=ReadIntProperty(sl,'Status')=1;
        end;
        Result:=True;
     finally
       sl.Free;
     end;
   finally
     strm.Free;
   end;
end;

function ReadOBDXFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
  try strm:=Sub1.OpenStream(Format(strm_OBDX,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(Self.FOBDX,Length(Self.FOBDX)+1);
        with Self.FOBDX[High(Self.FOBDX)] do begin
          DeviceName:=ReadStrProperty(sl,'DeviceName');
          Typ:=TSMBIOS_OnBoardDeviceType(ReadIntProperty(sl,'Typ'));
          Status:=ReadIntProperty(sl,'Status')=1;
          Instance:=ReadIntProperty(sl,'Instance');
          SegmentGroupNumber:=ReadIntProperty(sl,'SegmentGroupNumber');
          BusNumber:=ReadIntProperty(sl,'BusNumber');
          DeviceNumber:=ReadIntProperty(sl,'DeviceNumber');
          FunctionNumber:=ReadIntProperty(sl,'FunctionNumber');
        end;
        Result:=True;
     finally
       sl.Free;
     end;
   finally
     strm.Free;
   end;
end;

function ReadTempProbeFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
  try strm:=Sub1.OpenStream(Format(strm_TempProbe,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(Self.FTP,Length(Self.FTP)+1);
        with Self.FTP[High(Self.FTP)] do begin
          Description:=ReadStrProperty(sl,'Description');
          Location:=TSMBIOS_TempProbeLocationType(ReadIntProperty(sl,'Location'));
          Status:=TSMBIOS_StatusType(ReadIntProperty(sl,'Status'));
          Max:=ReadIntProperty(sl,'Max');
          Min:=ReadIntProperty(sl,'Min');
          Resolution:=ReadIntProperty(sl,'Resolution');
          Tolerance:=ReadIntProperty(sl,'Tolerance');
          Accuracy:=ReadIntProperty(sl,'Accuracy');
          NominalValue:=ReadIntProperty(sl,'NominalValue');
        end;
        Result:=True;
     finally
       sl.Free;
     end;
  finally
    strm.Free;
  end;
end;

function ReadVoltProbeFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
  try strm:=Sub1.OpenStream(Format(strm_VoltProbe,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(Self.FVP,Length(Self.FVP)+1);
        with Self.FVP[High(Self.FVP)] do begin
          Description:=ReadStrProperty(sl,'Description');
          Location:=TSMBIOS_VoltProbeLocationType(ReadIntProperty(sl,'Location'));
          Status:=TSMBIOS_StatusType(ReadIntProperty(sl,'Status'));
          Max:=ReadIntProperty(sl,'Max');
          Min:=ReadIntProperty(sl,'Min');
          Resolution:=ReadIntProperty(sl,'Resolution');
          Tolerance:=ReadIntProperty(sl,'Tolerance');
          Accuracy:=ReadIntProperty(sl,'Accuracy');
          NominalValue:=ReadIntProperty(sl,'NominalValue');
        end;
        Result:=True;
     finally
       sl.Free;
     end;
  finally
    strm.Free;
  end;
end;

function ReadCurrProbeFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
  try strm:=Sub1.OpenStream(Format(strm_CurrProbe,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(Self.FCP,Length(Self.FCP)+1);
        with Self.FCP[High(Self.FCP)] do begin
          Description:=ReadStrProperty(sl,'Description');
          Location:=TSMBIOS_CurrProbeLocationType(ReadIntProperty(sl,'Location'));
          Status:=TSMBIOS_StatusType(ReadIntProperty(sl,'Status'));
          Max:=ReadIntProperty(sl,'Max');
          Min:=ReadIntProperty(sl,'Min');
          Resolution:=ReadIntProperty(sl,'Resolution');
          Tolerance:=ReadIntProperty(sl,'Tolerance');
          Accuracy:=ReadIntProperty(sl,'Accuracy');
          NominalValue:=ReadIntProperty(sl,'NominalValue');
        end;
        Result:=True;
     finally
       sl.Free;
     end;
  finally
    strm.Free;
  end;
end;

function ReadCoolDevFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
  try strm:=Sub1.OpenStream(Format(strm_CoolDev,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(Self.FCD,Length(Self.FCD)+1);
        with Self.FCD[High(Self.FCD)] do begin
          Description:=ReadStrProperty(sl,'Description');
          Typ:=TSMBIOS_CoolingType(ReadIntProperty(sl,'Type'));
          Status:=TSMBIOS_StatusType(ReadIntProperty(sl,'Status'));
          GroupUnit:=ReadIntProperty(sl,'GroupUnit');
          TemperatureProbeHandle:=ReadIntProperty(sl,'TPHandle');
          NominalSpeed:=ReadIntProperty(sl,'Speed');
        end;
        Result:=True;
     finally
       sl.Free;
     end;
  finally
    strm.Free;
  end;
end;

function ReadTPMDevFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
  try strm:=Sub1.OpenStream(Format(strm_TPMD,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(Self.FTPM,Length(Self.FTPM)+1);
        with Self.FTPM[High(Self.FTPM)] do begin
          VendorID:=ReadStrProperty(sl,'VendorID');
          Description:=ReadStrProperty(sl,'Description');
          MajorSpecVersion:=ReadIntProperty(sl,'MajorSpecVersion');
          MinorSpecVersion:=ReadIntProperty(sl,'MinorSpecVersion');
          FirmwareVersion1:=ReadIntProperty(sl,'FirmwareVersion1');
          FirmwareVersion2:=ReadIntProperty(sl,'FirmwareVersion2');
          Chars:=ReadIntProperty(sl,'Chars');
          OEMDef:=ReadIntProperty(sl,'OEMDef');
        end;
        Result:=True;
     finally
       sl.Free;
     end;
  finally
    strm.Free;
  end;
end;

function ReadPortableBatteryFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
  try strm:=Sub1.OpenStream(Format(strm_BAT,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(Self.FBat,Length(Self.FBat)+1);
        with Self.FBat[High(Self.FBat)] do begin
          Location:=ReadStrProperty(sl,'Location');
          Manufacturer:=ReadStrProperty(sl,'Manufacturer');
          ManufacturerDate:=ReadStrProperty(sl,'ManufacturerDate');
          SerialNumber:=ReadStrProperty(sl,'SerialNumber');
          DeviceName:=ReadStrProperty(sl,'DeviceName');
          DeviceChemistry:=TSMBIOS_DeviceChemistry(ReadIntProperty(sl,'DeviceChemistry'));
          DesignCapacity:=ReadIntProperty(sl,'DesignCapacity');
          DesignVoltage:=ReadIntProperty(sl,'DesignVoltage');
          MaxErrorInBatData:=ReadIntProperty(sl,'MaxErrorInBatData');
          SBDSVersionNumber:=ReadStrProperty(sl,'SBDSVersionNumber');
          SBDSDeviceChemistry:=ReadStrProperty(sl,'SBDSDeviceChemistry');
          SBDSSerialNumber:=ReadIntProperty(sl,'SBDSSerialNumber');
          SBDSManufactureDate:=ReadIntProperty(sl,'SBDSManufactureDate');
          DesignCapacityMultiplier:=ReadIntProperty(sl,'DesignCapacityMultiplier');
          OEMSpec:=ReadIntProperty(sl,'OEMSpec');
        end;
        Result:=True;
     finally
       sl.Free;
     end;
  finally
    strm.Free;
  end;
end;

var
  i: Integer;
  strm: TStorageStream;
  sl: TStringList;
  p,n: integer;
  ms: TSMBIOS_MemorySpeed;
  mt: TSMBIOS_MemoryType;
  mv: TSMBIOS_Voltage;
  bc: TSMBIOS_BIOSChar;
  s: string;
begin
  Clear;
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    try
      Sub:=SS.OpenSubStorage(StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;

    if Sub<>nil then
    try
      strm:=Sub.OpenStream(strm_Props,STG_READ_INSTORAGE,False);
      if strm<>nil then
        try
          sl:=TStringList.Create;
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            Self.FRevisionMajor:=ReadIntProperty(sl,'RevisionMajor');
            Self.FRevisionMinor:=ReadIntProperty(sl,'RevisionMinor');
            s:=ReadStrProperty(sl,'Revision');
            p:=Pos('.',s);
            if p>0 then begin
              FRevisionMajor:=StrToIntDef(Copy(s,1,p-1),0);
              FRevisionMinor:=StrToIntDef(Copy(s,p+1),0);
            end;
            Self.FMajorVersion:=ReadIntProperty(sl,'MajorVersion');
            Self.FMinorVersion:=ReadIntProperty(sl,'MinorVersion');
            Self.FStart:=ReadIntProperty(sl,'SMBIOSAddress');
            Self.FStructStart:=ReadIntProperty(sl,'StructStart');
            Self.FLen:=ReadIntProperty(sl,'StructLength');
            Self.FTableCount:=ReadIntProperty(sl,'StructCount');
            Self.FBIOSVendor:=ReadStrProperty(sl,'BIOSVendor');
            Self.FBIOSVersion:=ReadStrProperty(sl,'BIOSVersion');
            Self.FBIOSdate:=ReadStrProperty(sl,'BIOSDate');
            Self.FBIOSSize:=ReadIntProperty(sl,'BIOSSize');
            Self.FBIOSMajor:=ReadIntProperty(sl,'BIOSMajorVersion');
            Self.FBIOSMinor:=ReadIntProperty(sl,'BIOSMinorVersion');
            Self.FBIOSECFMajor:=ReadIntProperty(sl,'BIOSECFMajorVersion');
            Self.FBIOSECFMinor:=ReadIntProperty(sl,'BIOSECFMinorVersion');
            n:=ReadIntProperty(sl,'BIOSCharacteristics');
            Self.FBIOSChars:=[];
            for bc:=Low(TSMBIOS_BIOSChar) to High(TSMBIOS_BIOSChar) do
              if n and Round(Power(2,Integer(bc)))<>0 then
                Self.FBIOSChars:=Self.FBIOSChars+[bc];
            Self.FSysMod:=ReadStrProperty(sl,'SystemModel');
            Self.FSysMan:=ReadStrProperty(sl,'SystemManufacturer');
            Self.FSysVer:=ReadStrProperty(sl,'SystemVersion');
            Self.FSysSN:=ReadStrProperty(sl,'SystemSerial');
            Self.FSysUUID:=ReadStrProperty(sl,'SystemUUID');
            Self.FMBMod:=ReadStrProperty(sl,'MainboardModel');
            Self.FMBMan:=ReadStrProperty(sl,'MainboardManufacturer');
            Self.FMBVer:=ReadStrProperty(sl,'MainboardVersion');
            Self.FMBSN:=ReadStrProperty(sl,'MainboardSerial');
            Self.FMBAT:=ReadStrProperty(sl,'MainboardAssetTag');
            Self.FMBLIC:=ReadStrProperty(sl,'MainboardLocationInChassis');
            Self.FCHMod:=TSMBIOS_Chassis(ReadIntProperty(sl,'ChassisModel'));
            Self.FCHMan:=ReadStrProperty(sl,'ChassisManufacturer');
            Self.FCHVer:=ReadStrProperty(sl,'ChassisVersion');
            Self.FCHSN:=ReadStrProperty(sl,'ChassisSerial');
            Self.FCHAT:=ReadStrProperty(sl,'ChassisAssetTag');
            Self.FMCSI:=TSMBIOS_InterleaveSupport(ReadIntProperty(sl,'MemCtrlCurrentInterleave'));
            Self.FMCSI:=TSMBIOS_InterleaveSupport(ReadIntProperty(sl,'MemCtrlSupportedInterleave'));
            Self.FMCMS:=ReadIntProperty(sl,'MemCtrlMaxSize');
            Self.FMCSC:=ReadIntProperty(sl,'MemCtrlSlotCount');
            n:=ReadIntProperty(sl,'MemCtrlSupportedSpeeds');
            Self.FMCSS:=[];
            for ms:=Low(TSMBIOS_MemorySpeed) to High(TSMBIOS_MemorySpeed) do
              if n and Round(Power(2,Integer(ms)))<>0 then
                Self.FMCSS:=Self.FMCSS+[ms];
            n:=ReadIntProperty(sl,'MemCtrlSupportedTypes');
            Self.FMCST:=[];
            for mt:=Low(TSMBIOS_MemoryType) to High(TSMBIOS_MemoryType) do
              if n and Round(Power(2,Integer(mt)))<>0 then
                Self.FMCST:=Self.FMCST+[mt];
            n:=ReadIntProperty(sl,'MemCtrlSupportedVoltages');
            Self.FMCSV:=[];
            for mv:=Low(TSMBIOS_Voltage) to High(TSMBIOS_Voltage) do
              if n and Round(Power(2,Integer(mv)))<>0 then
                Self.FMCSV:=Self.FMCSV+[mv];

            try
              Self.FPMALoc:=TSMBIOS_PMALocation(ReadIntProperty(sl,'PMALocation'));
              Self.FPMAUse:=TSMBIOS_PMAUse(ReadIntProperty(sl,'PMAUse'));
              Self.FPMAECT:=TSMBIOS_PMAErrorCorrectionType(ReadIntProperty(sl,'PMAErrorCorrectionType'));
              Self.FPMAMC:=ReadIntProperty(sl,'PMAMaximumCapacity');
              Self.FPMADN:=ReadIntProperty(sl,'PMANumberOfMemoryDevices');

              Self.FSPSPUG:=ReadIntProperty(sl,'SystemPowerSupplyPowerUnitGroup');
              Self.FSPSLoc:=ReadStrProperty(sl,'SystemPowerSupplyLocation');
              Self.FSPSDN:=ReadStrProperty(sl,'SystemPowerSupplyDeviceName');
              Self.FSPSM:=ReadStrProperty(sl,'SystemPowerSupplyManufacturer');
              Self.FSPSSN:=ReadStrProperty(sl,'SystemPowerSupplySerialNumber');
              Self.FSPSATN:=ReadStrProperty(sl,'SystemPowerSupplyAssetTagNumber');
              Self.FSPSMPN:=ReadStrProperty(sl,'SystemPowerSupplyModelPartNumber');
              Self.FSPSRL:=ReadStrProperty(sl,'SystemPowerSupplyRevisionLevel');
              Self.FSPSMPC:=ReadIntProperty(sl,'SystemPowerSupplyMaxPowerCapacity');
              Self.FSPSC:=ReadIntProperty(sl,'SystemPowerSupplyChars');
              Self.FSPSIVPH:=ReadIntProperty(sl,'SystemPowerSupplyInputVoltageProbeHandle');
              Self.FSPSCDH:=ReadIntProperty(sl,'SystemPowerSupplyCoolingDeviceHandle');
              Self.FSPSICPH:=ReadIntProperty(sl,'SystemPowerSupplyInputCurrenyProbeHandle');
            except
            end;
            SetDataAvail(True);
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;

      try
        Sub1:=Sub.OpenSubStorage(Processor_StorageFolderName,STG_READ_INSTORAGE,False);
      except
        Sub1:=nil;
      end;
      try
        i:=0;
        while ReadProcessorFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        if Sub1<>nil then
         Sub1.Free;
      end;

      try
        Sub1:=Sub.OpenSubStorage(MemoryModule_StorageFolderName,STG_READ_INSTORAGE,False);
      except
        Sub1:=nil;
      end;
      try
        i:=0;
        while ReadMemModFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        if Sub1<>nil then
         Sub1.Free;
      end;

      try
        Sub1:=Sub.OpenSubStorage(MemoryDevice_StorageFolderName,STG_READ_INSTORAGE,False);
      except
        Sub1:=nil;
      end;
      try
        i:=0;
        while ReadMemDevFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        if Sub1<>nil then
         Sub1.Free;
      end;

      try
        Sub1:=Sub.OpenSubStorage(Port_StorageFolderName,STG_READ_INSTORAGE,False);
      except
        Sub1:=nil;
      end;
      try
        i:=0;
        while ReadPortFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        if Sub1<>nil then
         Sub1.Free;
      end;

      try
        Sub1:=Sub.OpenSubStorage(SystemSlot_StorageFolderName,STG_READ_INSTORAGE,False);
      except
        Sub1:=nil;
      end;
      try
        i:=0;
        while ReadSystemSlotFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        if Sub1<>nil then
         Sub1.Free;
      end;

      try
        Sub1:=Sub.OpenSubStorage(Cache_StorageFolderName,STG_READ_INSTORAGE,False);
      except
        Sub1:=nil;
      end;
      try
        i:=0;
        while ReadCacheFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        if Sub1<>nil then
         Sub1.Free;
      end;

      try
        Sub1:=Sub.OpenSubStorage(OnBoardDevice_StorageFolderName,STG_READ_INSTORAGE,False);
      except
        Sub1:=nil;
      end;
      try
        i:=0;
        while ReadOBDFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        if Sub1<>nil then
         Sub1.Free;
      end;

      try
        Sub1:=Sub.OpenSubStorage(OnBoardDeviceEx_StorageFolderName,STG_READ_INSTORAGE,False);
      except
        Sub1:=nil;
      end;
      try
        i:=0;
        while ReadOBDXFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        if Sub1<>nil then
         Sub1.Free;
      end;

      try
        Sub1:=Sub.OpenSubStorage(TemperatureProbe_StorageFolderName,STG_READ_INSTORAGE,False);
      except
        Sub1:=nil;
      end;
      try
        i:=0;
        while ReadTempProbeFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        if Sub1<>nil then
         Sub1.Free;
      end;

      try
        Sub1:=Sub.OpenSubStorage(VoltageProbe_StorageFolderName,STG_READ_INSTORAGE,False);
      except
        Sub1:=nil;
      end;
      try
        i:=0;
        while ReadVoltProbeFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        if Sub1<>nil then
         Sub1.Free;
      end;

      try
        Sub1:=Sub.OpenSubStorage(CurrentProbe_StorageFolderName,STG_READ_INSTORAGE,False);
      except
        Sub1:=nil;
      end;
      try
        i:=0;
        while ReadCurrProbeFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        if Sub1<>nil then
         Sub1.Free;
      end;

      try
        Sub1:=Sub.OpenSubStorage(CoolingDevice_StorageFolderName,STG_READ_INSTORAGE,False);
      except
        Sub1:=nil;
      end;
      try
        i:=0;
        while ReadCoolDevFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        if Sub1<>nil then
         Sub1.Free;
      end;

      try
        Sub1:=Sub.OpenSubStorage(TPMDevice_StorageFolderName,STG_READ_INSTORAGE,False);
      except
        Sub1:=nil;
      end;
      try
        i:=0;
        while ReadTPMDevFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        if Sub1<>nil then
         Sub1.Free;
      end;

      try
        Sub1:=Sub.OpenSubStorage(PortableBattery_StorageFolderName,STG_READ_INSTORAGE,False);
      except
        Sub1:=nil;
      end;
      try
        i:=0;
        while ReadPortableBatteryFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        if Sub1<>nil then
         Sub1.Free;
      end;
    finally
      if Sub<>nil then
        Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_SMBIOS.LoadRawMemoryFromFile(const AFilename: string);
begin
  FStructure.Clear;
  FSMBIOS.LoadFromFile(AFilename);
  FLocal:=False;
  RefreshData;
end;

procedure TMiTeC_SMBIOS.LoadSMBIOSFromFile(const AFilename: string);
begin
  FSMBIOS.Clear;
  FStructure.LoadFromFile(AFilename);
  FLocal:=False;
  RefreshData;
end;

function TMiTeC_SMBIOS.GetOBDCount: Byte;
begin
  Result:=Length(FOBD);
end;

function TMiTeC_SMBIOS.GetOBDXCount: Byte;
begin
  Result:=Length(FOBDX);
end;

function TMiTeC_SMBIOS.GetOBDX(Index: Byte): TSMBIOS_OnBoardDeviceEx;
begin
  Finalize(Result);
  ResetMemory(Result,SizeOf(Result));
  try
    Result:=FOBDX[Index];
  except
  end;
end;

function TMiTeC_SMBIOS.GetOBD(Index: Byte): TSMBIOS_OnBoardDevice;
begin
  Finalize(Result);
  ResetMemory(Result,SizeOf(Result));
  try
    Result:=FOBD[Index];
  except
  end;
end;

function TMiTeC_SMBIOS.GetTempProbe(Index: Byte): TSMBIOS_TemperatureProbe;
begin
  Finalize(Result);
  ResetMemory(Result,SizeOf(Result));
  try
    Result:=FTP[Index];
  except
  end;
end;

function TMiTeC_SMBIOS.GetTempProbeCount: Byte;
begin
  Result:=Length(FTP);
end;

function TMiTeC_SMBIOS.GetTPMD(Index: Byte): TSMBIOS_TPMDevice;
begin
  Finalize(Result);
  ResetMemory(Result,SizeOf(Result));
  try
    Result:=FTPM[Index];
  except
  end;
end;

function TMiTeC_SMBIOS.GetTPMDCount: Byte;
begin
  Result:=Length(FTPM);
end;

function TMiTeC_SMBIOS.GetVoltProbe(Index: Byte): TSMBIOS_VoltageProbe;
begin
  Finalize(Result);
  ResetMemory(Result,SizeOf(Result));
  try
    Result:=FVP[Index];
  except
  end;
end;

function TMiTeC_SMBIOS.GetVoltProbeCount: Byte;
begin
  Result:=Length(FVP);
end;

function TMiTeC_SMBIOS.GetMemModCount: Byte;
begin
  Result:=Length(FMemMod);
end;

function TMiTeC_SMBIOS.GetMemoryDevice(Index: Byte): TSMBIOS_MemoryDevice;
begin
  Finalize(Result);
  ResetMemory(Result,SizeOf(Result));
  try
    Result:=FMemDev[Index];
  except
  end;
end;

end.
