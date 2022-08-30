{*******************************************************}
{       MiTeC System Information Component Suite        }
{                CPU Detection Part                     }
{                  version 13.4.0                       }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_CPU;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj,
     {$ENDIF}
     MiTeC_Windows, MiTeC_SS, MSI_Common, MSI_Defs, MiTeC_Routines;

const
  StorageFolderName = 'CPU';

procedure SetProcAffinity(FIndex: Byte);
procedure RestoreProcAffinity;

type
  TIntelBrand = (ibCeleron, ibPentium, ibXeon, ibMP, ibMobile, ibM, ibDuoCore, ibP4);
  TIntelBrands = set of TIntelBrand;

const
  CPUID_EFlags         = $00200000;
  CPUID_OpCode         = $0000A20F;

  CPUID_STD_MaximumLevel     = $00000000;
  CPUID_STD_VendorSignature  = $00000000;
  CPUID_STD_Signature        = $00000001;
  CPUID_STD_FeatureSet       = $00000001;
  CPUID_STD_CacheTlbs        = $00000002;
  CPUID_STD_SerialNumber     = $00000003;
  CPUID_STD_CacheParams      = $00000004;
  CPUID_STD_MonitorMWAIT     = $00000005;
  CPUID_STD_ThermalPower     = $00000006;
  CPUID_STD_DCA              = $00000009;
  CPUID_STD_ArcPerfMon       = $0000000A;
  CPUID_STD_Topology         = $0000000B;
  CPUID_STD_XSAVE            = $0000000D;

  CPUID_EXT_MaximumLevel     = $80000000;
  CPUID_EXT_Signature        = $80000001;
  CPUID_EXT_FeatureSet       = $80000001;
  CPUID_EXT_MarketingName1   = $80000002;
  CPUID_EXT_MarketingName2   = $80000003;
  CPUID_EXT_MarketingName3   = $80000004;
  CPUID_EXT_Level1Cache      = $80000005;
  CPUID_EXT_Level2Cache      = $80000006;
  CPUID_EXT_PowerManagement  = $80000007;
  CPUID_EXT_AA64Information  = $80000008;
  CPUID_EXT_Unsupported      = $80000099;  // Dummy command for unsuported features

  CPUID_TMX_MaximumLevel     = $80860000;
  CPUID_TMX_Signature        = $80860001;
  CPUID_TMX_SoftwareVersion  = $80860002;
  CPUID_TMX_MarketingName1   = $80860003;
  CPUID_TMX_MarketingName2   = $80860004;
  CPUID_TMX_MarketingName3   = $80860005;
  CPUID_TMX_MarketingName4   = $80860006;
  CPUID_TMX_Operation        = $80860007;

  INITIAL_APIC_ID_BITS       = $FF000000;
  NUM_LOGICAL_BITS           = $00FF0000;
  PHY_ID_MASK                = $FF;
  PHY_ID_SHIFT               = 0;

type
  TCPUIDExecutionLevel = (celStandard, celExtended, celTransmeta);

  TCPUIDResult = record
    EAX,
    EBX,
    ECX,
    EDX: {$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF};
  end;

var
  CPUIDCommand, CPUIDLevel: Cardinal;

function GetCPUIDResult: TCPUIDResult;
function IsCPUIDSupported(Cpu: Byte): Boolean;
function GetCPUIDCommandLevel(Command: Cardinal): TCPUIDExecutionLevel;
function IsCPUIDCommandSupported(Cpu: Byte; Command: Cardinal): Boolean;
function GetCPUIDMaximumCommand(Cpu: Byte; Level: TCPUIDExecutionLevel): {$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF};
function ExecuteCPUID(Cpu: Integer; Command: Cardinal; Iterations: Integer = 1; Level: Cardinal = 0): TCPUIDResult;

type
  TFeatureAvailability = (faCommon, faIntel, faAmd, faCyrix);

  TFeatureSet = (fsUnknown, fsStandard1, fsStandard2, fsExtended1, fsExtended2, fsPowerManagement);

  TFeatureLevel = set of TFeatureSet;

  TFeatureDetail = record
    Index: Byte;
    Mnemonic: String;
    Name: String;
    Info: TFeatureAvailability;
    Level: TFeatureLevel;
  end;

  TCPUFeature = record
    Index: Byte;
    Mnemonic: String;
    Name: String;
    Level: TFeatureSet;
    Available: Boolean;
  end;

  TAvailableFeatures = array of TCPUFeature;

const
  cFeatureDetails: array[0..54] of TFeatureDetail = (
// standard (EDX)
    (Index: 0;  Mnemonic: 'FPU';    Name: 'Floating point unit';
                Info: faCommon;   Level: [fsStandard1,fsExtended1]),
    (Index: 1;  Mnemonic: 'VME';    Name: 'Virtual mode extension';
                Info: faCommon;   Level: [fsStandard1,fsExtended1]),
    (Index: 2;  Mnemonic: 'DE';     Name: 'Debugging extensions';
                Info: faCommon;   Level: [fsStandard1,fsExtended1]),
    (Index: 3;  Mnemonic: 'PSE';    Name: 'Page size extension';
                Info: faCommon;   Level: [fsStandard1,fsExtended1]),
    (Index: 4;  Mnemonic: 'TSC';    Name: 'Time stamp counter';
                Info: faCommon;   Level: [fsStandard1,fsExtended1]),
    (Index: 5;  Mnemonic: 'MSR';    Name: 'Machine specific registers';
                Info: faCommon;   Level: [fsStandard1,fsExtended1]),
    (Index: 6;  Mnemonic: 'PAE';    Name: 'Physical address extension';
                Info: faCommon;   Level: [fsStandard1,fsExtended1]),
    (Index: 7;  Mnemonic: 'MCE';    Name: 'Machine check extension';
                Info: faCommon;   Level: [fsStandard1,fsExtended1]),
    (Index: 8;  Mnemonic: 'CX8';    Name: 'CMPXCHG8 instrucion support';
                Info: faCommon;   Level: [fsStandard1,fsExtended1]),
    (Index: 9;  Mnemonic: 'APIC';   Name: 'APIC';
                Info: faCommon;   Level: [fsStandard1]),
    (Index: 11; Mnemonic: 'SEP';    Name: 'Fast system call (SYSENTER/SYSEXIT)';
                Info: faCommon;   Level: [fsStandard1,fsExtended1]),
    (Index: 12; Mnemonic: 'MTRR';   Name: 'Memory type range registers';
                Info: faCommon;   Level: [fsStandard1,fsExtended1]),
    (Index: 13; Mnemonic: 'PGE';    Name: 'Page global extension';
                Info: faCommon;   Level: [fsStandard1]),
    (Index: 15; Mnemonic: 'CMOV';   Name: 'Conditional move support';
                Info: faCommon;   Level: [fsStandard1,fsExtended1]),
    (Index: 16; Mnemonic: 'PAT';    Name: 'Page attribute table';
                Info: faCommon;      Level: [fsStandard1,fsExtended1]),
    (Index: 17; Mnemonic: 'PSE36';  Name: '36-bit page size extension';
                Info: faCommon;   Level: [fsStandard1,fsExtended1]),
    (Index: 18; Mnemonic: 'PSN';    Name: 'Processor serial number';
                Info: faCommon;   Level: [fsStandard1]),
    (Index: 19; Mnemonic: 'CLFSH';  Name: 'CLFLUSH instruction support';
                Info: faCommon;   Level: [fsStandard1]),
    (Index: 21; Mnemonic: 'DS';   Name: 'Debug trace store';
                Info: faCommon;   Level: [fsStandard1]),
    (Index: 22; Mnemonic: 'ACPI';   Name: 'Thermal monitor and software controlled clock';
                Info: faCommon;   Level: [fsStandard1]),
    (Index: 23; Mnemonic: 'MMX';    Name: 'MMX architecture support';
                Info: faCommon;   Level: [fsStandard1,fsExtended1]),
    (Index: 24; Mnemonic: 'FXSR';   Name: 'Fast floating point save (FXSAVE/FXRSTOR)';
                Info: faCommon;   Level: [fsStandard1]),
    (Index: 25; Mnemonic: 'SSE';    Name: 'Streaming SIMD instruction support';
                Info: faCommon;   Level: [fsStandard1]),
    (Index: 26; Mnemonic: 'SSE2';   Name: 'Streaming SIMD extensions 2';
                Info: faCommon;   Level: [fsStandard1,fsExtended1]),
    (Index: 27; Mnemonic: 'SS';     Name: 'Self snoop';
                Info: faCommon;   Level: [fsStandard1]),
    (Index: 28; Mnemonic: 'HTT';    Name: 'Hyper-Threading technology';
                Info: faCommon;   Level: [fsStandard1]),
    (Index: 29; Mnemonic: 'TM';     Name: 'Thermal monitor support';
                Info: faCommon;   Level: [fsStandard1]),
    (Index: 30; Mnemonic: 'IA-64';  Name: 'IA-64 Intel';
                Info: faCommon;   Level: [fsStandard1,fsExtended1]),
    (Index: 31; Mnemonic: 'SBF';    Name: 'Signal break on FERR';
                Info: faCommon;   Level: [fsStandard1]),
// standard (ECX)
    (Index: 0;  Mnemonic: 'SSE3';   Name: 'Streaming SIMD extensions 3';
                Info: faCommon;   Level: [fsStandard2]),
    (Index: 3;  Mnemonic: 'MON';    Name: 'MONITOR/MWAIT';
                Info: faCommon;   Level: [fsStandard2]),
    (Index: 4;  Mnemonic: 'CPL';    Name: 'CPL qualified debug store';
                Info: faCommon;   Level: [fsStandard2]),
    (Index: 5;  Mnemonic: 'VMX';    Name: 'Virtual machine extension';
                Info: faCommon;   Level: [fsStandard2]),
    (Index: 7;  Mnemonic: 'EST';    Name: 'Enhanced SpeedStep Technology';
                Info: faCommon;   Level: [fsStandard2]),
    (Index: 8;  Mnemonic: 'TM2';    Name: 'Thermal Monitor 2';
                Info: faCommon;   Level: [fsStandard2]),
    (Index: 10; Mnemonic: 'CID';    Name: 'Context Id';
                Info: faCommon;   Level: [fsStandard2]),
    (Index: 13; Mnemonic: 'CX16';   Name: 'CMPXCHG16B instrucion support';
                Info: faCommon;   Level: [fsStandard2]),
    (Index: 14; Mnemonic: 'xTPR';   Name: 'Send task priority messages';
                Info: faCommon;   Level: [fsStandard2]),
// extended (EDX)
     (Index: 9;  Mnemonic: 'APIC';   Name: 'On-chip APIC hardware';
                Info: faCommon;   Level: [fsExtended1]),
     (Index: 13; Mnemonic: 'PGE';    Name: 'Page global enable';
                Info: faCommon;   Level: [fsExtended1]),
     (Index: 19; Mnemonic: 'MP';  Name: 'Multiprocessing capable';
                Info: faCommon;   Level: [fsExtended1]),
     (Index: 20; Mnemonic: 'NX';  Name: 'No execute page protection';
                Info: faCommon;   Level: [fsExtended1]),
     (Index: 22; Mnemonic: 'MMX+';   Name: 'Extended MMX architecture';
                Info: faCommon;   Level: [fsExtended1]),
     (Index: 29; Mnemonic: 'x64';     Name: 'AMD64/EM64T';
                Info: faCommon;   Level: [fsExtended1]),
     (Index: 30; Mnemonic: '3DNOW+'; Name: 'Extended 3DNow! extensions';
                Info: faCommon;      Level: [fsExtended1]),
      (Index: 31; Mnemonic: '3DNOW';  Name: '3DNow! extensions';
                Info: faCommon;      Level: [fsExtended1]),
// extended (ECX)
    (Index: 0;  Mnemonic: 'LSAHF';   Name: 'LAHF/SAHF support';
                Info: faCommon;   Level: [fsExtended2]),
    (Index: 1;  Mnemonic: 'CMPL';    Name: 'Core multiprocessing legacy';
                Info: faCommon;      Level: [fsExtended2]),
// power management (EDX)
    (Index: 0;  Mnemonic: 'TS';     Name: 'Temperature sensor';
                Info: faCommon;      Level: [fsPowerManagement]),
    (Index: 1;  Mnemonic: 'FID';    Name: 'Frequency id Control';
                Info: faCommon;      Level: [fsPowerManagement]),
    (Index: 2;  Mnemonic: 'VID';    Name: 'Voltage id Control';
                Info: faCommon;      Level: [fsPowerManagement]),
    (Index: 3;  Mnemonic: 'TPP';    Name: 'Thermal trip';
                Info: faCommon;      Level: [fsPowerManagement]),
    (Index: 4;  Mnemonic: 'TM';     Name: 'Thermal monitor support';
                Info: faCommon;      Level: [fsPowerManagement]),
    (Index: 5;  Mnemonic: 'STC';    Name: 'Software thermal control';
                Info: faCommon;      Level: [fsPowerManagement]),
    (Index: 8;  Mnemonic: 'ITSC';    Name: 'Invariant TSC';
                Info: faCommon;      Level: [fsPowerManagement])

  );

  SFS_APIC   = 9;
  SFS_SEPK6  = 10;
  SFS_PSN    = 18;
  SFS_HTT    = 28;
  SFS_IA64   = 30;
  EFS_MP     = 19;   // AMD MULTI-PROCESSOR
  EFS_MMXAMD = 22;   // AMD MMX EXTENSIONS
  EFS_MMXVIA = 24;   // CYRIX/VIA MMX EXTENSIONS

  cExtend_Fields      = 15;

  rsGenericName_x86 = 'x86 Family %d Model %d Stepping %d';
  rsGenericName_x64 = 'Intel64 Family %d Model %d Stepping %d';
  rsGenericName_ia64 = 'ia64 Family %d Model %d Stepping %d';

type
  TCpuVendor = (cvNone, cvUnknown, cvIntel, cvAmd, cvCyrix, cvIDT, cvNexGen, cvUMC, cvRise, cvSiS, cvGeode, cvTransmeta);

  TVendorCacheDetect = (vcdStandard, vcdExtended, vcdCombined);

  TCacheDescriptors = array[1..16] of Cardinal;

  TCacheSegment = (csCode, csData, csUnified);

  TCPUVendorInfo = record
    Signature: String;
    Prefix: String;
    Name: String;
    FeatureAvailability: TFeatureAvailability;
    CacheDetect: TVendorCacheDetect;
  end;

  TCacheLevel = (clLevel1Code, clLevel1Data, clLevel1Unified, clCodeTLB, clDataTLB, clUnifiedTLB,
                 clLevel2, clLevel3, clTrace, clNone);

const
  ctDataCache    = 1;
  ctCodeCache    = 2;
  ctUnifiedCache = 3;

type
  TCacheAssociativity = (caNone, caDirect, ca2Way, ca4Way, ca8Way, ca12Way, ca16Way, caFull, ca6Way, ca24way);

  TCacheDescriptorInfo = record
    Descriptor: Byte;
    Level: TCacheLevel;
    Associativity: TCacheAssociativity;
    Size: Integer;
    LineSize: Integer;
    Description: String;
  end;

const
  cAssociativityInfo: array[caNone..ca24way] of Byte = (0, 1, 2, 4, 6, 7, 8, 15, 6, 24);
  cAssociativityDescription: array[caNone..ca24way] of String = ('None', 'Direct', '2-way', '4-way',
                                                                '8-way', '12-way', '16-way', 'Full', '6-way','24-way');

  cDescriptorInfo: array[0..103] of TCacheDescriptorInfo = (
    (Descriptor: $01; Level: clCodeTLB;     Associativity: ca4Way;   Size: 4;    LineSize: 32;  Description: 'Code TLB, 4K pages, 4 ways, 32 entries'),
    (Descriptor: $02; Level: clCodeTLB;     Associativity: caFull;   Size: 4096; LineSize: 2;   Description: 'Code TLB, 4M pages, fullway, 2 entries'),
    (Descriptor: $03; Level: clDataTLB;     Associativity: ca4Way;   Size: 4;    LineSize: 64;  Description: 'Data TLB, 4K pages, 4 ways, 64 entries'),
    (Descriptor: $04; Level: clDataTLB;     Associativity: ca4Way;   Size: 4096; LineSize: 8;   Description: 'Data TLB, 4M pages, 4 ways, 8 entries'),
    (Descriptor: $05; Level: clDataTLB;     Associativity: ca4Way;   Size: 4096; LineSize: 32;  Description: 'Data TLB, 4M pages, 4 ways, 32 entries'),
    (Descriptor: $06; Level: clLevel1Code;  Associativity: ca4Way;   Size: 8;    LineSize: 32;  Description: 'Code L1 Cache, 8K, 4 ways, 32b lines'),
    (Descriptor: $08; Level: clLevel1Code;  Associativity: ca4Way;   Size: 16;   LineSize: 32;  Description: 'Code L1 Cache, 16K, 4 ways, 32b lines'),
    (Descriptor: $0A; Level: clLevel1Data;  Associativity: ca2Way;   Size: 8;    LineSize: 32;  Description: 'Data L1 Cache, 8K, 2 ways, 32b lines'),
    (Descriptor: $0B; Level: clCodeTLB;     Associativity: ca4Way;   Size: 4096; LineSize: 4;   Description: 'Code TLB, 4M pages, 4 ways, 4 entries'),
    (Descriptor: $0C; Level: clLevel1Data;  Associativity: ca4Way;   Size: 16;   LineSize: 32;  Description: 'Data L1 Cache, 16K, 4 ways, 32b lines'),
    (Descriptor: $10; Level: clLevel1Data;  Associativity: ca4Way;   Size: 16;   LineSize: 32;  Description: 'Data L1 Cache, 16 KB, 4 ways, 32 byte lines (IA-64)'),
    (Descriptor: $15; Level: clLevel1Code;  Associativity: ca4Way;   Size: 16;   LineSize: 32;  Description: 'Code L1 Cache, 16 KB, 4 ways, 32 byte lines (IA-64)'),
    (Descriptor: $1A; Level: clLevel2;      Associativity: ca6Way;   Size: 96;   LineSize: 64;  Description: 'Unified L2 Cache, 96 KB, 6 ways, 64 byte lines (IA-64)'),
    (Descriptor: $22; Level: clLevel3;      Associativity: ca4Way;   Size: 512;  LineSize: 64;  Description: 'Unified L3 Cache, 512K, 4 ways, 64b lines'),
    (Descriptor: $23; Level: clLevel3;      Associativity: ca8Way;   Size: 1024; LineSize: 64;  Description: 'Unified L3 Cache, 1024K, 8 ways, 64b lines'),
    (Descriptor: $25; Level: clLevel3;      Associativity: ca8Way;   Size: 2048; LineSize: 64;  Description: 'Unified L3 Cache, 2048K, 8 ways, 64b lines'),
    (Descriptor: $29; Level: clLevel3;      Associativity: ca8Way;   Size: 4096; LineSize: 64;  Description: 'Unified L3 Cache, 4096K, 8 ways, 64b lines'),
    (Descriptor: $2C; Level: clLevel1Data;  Associativity: ca8Way;   Size: 32;   LineSize: 64;  Description: 'Data L1 Cache, 32 KB, 8 ways, 64 byte lines'),
    (Descriptor: $30; Level: clLevel1Code;  Associativity: ca8Way;   Size: 32;   LineSize: 64;  Description: 'Code L1 Cache, 32 KB, 8 ways, 64 byte lines'),
    (Descriptor: $39; Level: clLevel2;      Associativity: ca4Way;   Size: 128;  LineSize: 64;  Description: 'Unified L2 Cache, 128 KB, 4 ways, 64 byte lines, sectored'),
    (Descriptor: $3A; Level: clLevel2;      Associativity: ca6Way;   Size: 192;  LineSize: 64;  Description: 'Unified L2 Cache, 192 KB, 6 ways, 64 byte lines, sectored'),
    (Descriptor: $3B; Level: clLevel2;      Associativity: ca2Way;   Size: 128;  LineSize: 64;  Description: 'Unified L2 Cache, 128 KB, 2 ways, 64 byte lines, sectored'),
    (Descriptor: $3C; Level: clLevel2;      Associativity: ca4Way;   Size: 256;  LineSize: 64;  Description: 'Unified L2 Cache, 256B, 4 ways, 64 byte lines, sectored'),
    (Descriptor: $3D; Level: clLevel2;      Associativity: ca6Way;   Size: 384;  LineSize: 64;  Description: 'Unified L2 Cache, 384 KB, 6 ways, 64 byte lines, sectored'),
    (Descriptor: $3E; Level: clLevel2;      Associativity: ca4Way;   Size: 512;  LineSize: 64;  Description: 'Unified L2 Cache, 512 KB, 4 ways, 64 byte lines, sectored'),
//    (Descriptor: $40; Level: clLevel2;      Associativity: caNone;   Size: 0;    LineSize: 0;   Description: 'non integrated L2/L3 Cache'),
    (Descriptor: $41; Level: clLevel2;      Associativity: ca4Way;   Size: 128;  LineSize: 32;  Description: 'Unified L2 Cache, 128K, 4 ways, 32b lines'),
    (Descriptor: $42; Level: clLevel2;      Associativity: ca4Way;   Size: 256;  LineSize: 32;  Description: 'Unified L2 Cache, 256K, 4 ways, 32b lines'),
    (Descriptor: $43; Level: clLevel2;      Associativity: ca4Way;   Size: 512;  LineSize: 32;  Description: 'Unified L2 Cache, 512K, 4 ways, 32b lines'),
    (Descriptor: $44; Level: clLevel2;      Associativity: ca4Way;   Size: 1024; LineSize: 32;  Description: 'Unified L2 Cache, 1024K, 4 ways, 32b lines'),
    (Descriptor: $45; Level: clLevel2;      Associativity: ca4Way;   Size: 2048; LineSize: 32;  Description: 'Unified L2 Cache, 2048K, 4 ways, 32b lines'),
    (Descriptor: $46; Level: clLevel3;      Associativity: ca4Way;   Size: 4096; LineSize: 64;  Description: 'Unified L3 Cache, 4096 KB, 4 ways, 64 byte lines'),
    (Descriptor: $47; Level: clLevel3;      Associativity: ca8Way;   Size: 8192; LineSize: 64;  Description: 'Unified L3 Cache, 8192 KB, 8 ways, 64 byte lines'),
    (Descriptor: $49; Level: clLevel2;      Associativity: ca16Way;  Size: 4096; LineSize: 64;  Description: 'Unified L2 Cache, 4096 KB, 16 ways, 64 byte lines (Core 2)'),
    (Descriptor: $49; Level: clLevel3;      Associativity: ca16Way;  Size: 4096; LineSize: 64;  Description: 'Unified L3 Cache, 4096 KB, 16 ways, 64 byte lines (P4)'),
    (Descriptor: $4A; Level: clLevel3;      Associativity: ca12Way;  Size: 6144; LineSize: 64;  Description: 'Unified L3 Cache, 6144 KB, 12 ways, 64 byte lines'),
    (Descriptor: $4B; Level: clLevel3;      Associativity: ca16Way;  Size: 8192; LineSize: 64;  Description: 'Unified L3 Cache, 8192 KB, 16 ways, 64 byte lines'),
    (Descriptor: $4C; Level: clLevel3;      Associativity: ca12Way;  Size: 12288;LineSize: 64;  Description: 'Unified L3 Cache, 12288 KB, 12 ways, 64 byte lines'),
    (Descriptor: $4D; Level: clLevel3;      Associativity: ca16Way;  Size: 16384;LineSize: 64;  Description: 'Unified L3 Cache, 16384 KB, 16 ways, 64 byte lines'),
    (Descriptor: $50; Level: clCodeTLB;     Associativity: caNone;   Size: 0;    LineSize: 64;  Description: 'Code TLB, all pages, 64 entries'),
    (Descriptor: $51; Level: clCodeTLB;     Associativity: caNone;   Size: 0;    LineSize: 128; Description: 'Code TLB, all pages, 128 entries'),
    (Descriptor: $52; Level: clCodeTLB;     Associativity: caNone;   Size: 0;    LineSize: 512; Description: 'Code TLB, all pages, 512 entries'),
    (Descriptor: $56; Level: clDataTLB;     Associativity: ca4way;   Size: 4096; LineSize: 16;  Description: 'L0 Data TLB, 4M pages, 4 ways, 16 entries'),
    (Descriptor: $57; Level: clDataTLB;     Associativity: ca4way;   Size: 4096; LineSize: 16;  Description: 'L0 Data TLB, 4M pages, 4 ways, 16 entries'),
    (Descriptor: $5B; Level: clDataTLB;     Associativity: caNone;   Size: 0;    LineSize: 64;  Description: 'Data TLB, all pages, 64 entries'),
    (Descriptor: $5C; Level: clDataTLB;     Associativity: caNone;   Size: 0;    LineSize: 128; Description: 'Data TLB, all pages, 128 entries'),
    (Descriptor: $5D; Level: clDataTLB;     Associativity: caNone;   Size: 0;    LineSize: 256; Description: 'Data TLB, all pages, 256 entries'),
    (Descriptor: $60; Level: clLevel1Data;  Associativity: ca8Way;   Size: 16;   LineSize: 64;  Description: 'Data L1 Cache, 16K, 8 ways, 64 byte lines, sectored'),
    (Descriptor: $66; Level: clLevel1Data;  Associativity: ca4Way;   Size: 8;    LineSize: 64;  Description: 'Data L1 Cache, 8K, 4 ways'),
    (Descriptor: $67; Level: clLevel1Data;  Associativity: ca4Way;   Size: 16;   LineSize: 64;  Description: 'Data L1 Cache, 16K, 4 ways'),
    (Descriptor: $68; Level: clLevel1Data;  Associativity: ca4Way;   Size: 32;   LineSize: 64;  Description: 'Data L1 Cache, 32K, 4 ways'),
    (Descriptor: $70; Level: clUnifiedTLB;  Associativity: ca4Way;   Size: 4;    LineSize: 32;  Description: 'Unified TLB, 4k pages, 4 ways, 32 entries'),    //Cyrix
    (Descriptor: $70; Level: clTrace;       Associativity: ca8Way;   Size: 12;   LineSize: 0;   Description: 'Trace L1 Cache, 12 KµOPs, 4 ways'),
    (Descriptor: $71; Level: clTrace;       Associativity: ca8Way;   Size: 16;   LineSize: 0;   Description: 'Trace L1 Cache, 16 KµOPs, 4 ways'),
    (Descriptor: $72; Level: clTrace;       Associativity: ca8Way;   Size: 32;   LineSize: 0;   Description: 'Trace L1 Cache, 32 KµOPs, 4 ways'),
    (Descriptor: $73; Level: clTrace;       Associativity: ca8Way;   Size: 64;   LineSize: 0;   Description: 'Trace L1 Cache, 64 KµOPs, 8 ways'),
    (Descriptor: $77; Level: clLevel1Code;  Associativity: ca4Way;   Size: 16;   LineSize: 64;  Description: 'Code L1 Cache, 16 KB, 4 ways, 64 byte lines, sectored (IA-64)'),
    (Descriptor: $78; Level: clLevel2;      Associativity: ca4Way;   Size: 1024; LineSize: 64;  Description: 'Unified L2 Cache, 1024K, 4 ways, 64 byte lines'),
    (Descriptor: $79; Level: clLevel2;      Associativity: ca8Way;   Size: 128;  LineSize: 64;  Description: 'Unified L2 Cache, 128K, 8 ways, 32b lines, dual-sectored'),
    (Descriptor: $7A; Level: clLevel2;      Associativity: ca8Way;   Size: 256;  LineSize: 64;  Description: 'Unified L2 Cache, 512K, 8 ways, 32b lines, dual-sectored'),
    (Descriptor: $7B; Level: clLevel2;      Associativity: ca8Way;   Size: 512;  LineSize: 64;  Description: 'Unified L2 Cache, 1024K, 8 ways, 32b lines, dual-sectored'),
    (Descriptor: $7C; Level: clLevel2;      Associativity: ca8Way;   Size: 1024; LineSize: 64;  Description: 'Unified L2 Cache, 1024K, 8 ways, 64 byte lines, dual-sectored'),
    (Descriptor: $7D; Level: clLevel2;      Associativity: ca8Way;   Size: 2048; LineSize: 64;  Description: 'Unified L2 Cache, 2048K, 8 ways, 64 byte lines'),
    (Descriptor: $7E; Level: clLevel2;      Associativity: ca8Way;   Size: 1024; LineSize: 128; Description: 'Unified L2 Cache, 256 KB, 8 ways, 128 byte lines, sect. (IA-64)'),
    (Descriptor: $7F; Level: clLevel2;      Associativity: ca2Way;   Size: 512;  LineSize: 64;  Description: 'Unified L2 Cache, 512 KB, 2 ways, 64 byte lines'),
    (Descriptor: $80; Level: clUnifiedTLB;  Associativity: ca4Way;   Size: 16;   LineSize: 32;  Description: 'Unified TLB, 16k pages, 4 ways, 32 entries'),  //Cyrix
    (Descriptor: $81; Level: clLevel2;      Associativity: ca8Way;   Size: 128;  LineSize: 32;  Description: 'Unified L2 Cache, 128K, 8 ways, 32 byte lines'),
    (Descriptor: $82; Level: clLevel2;      Associativity: ca8Way;   Size: 256;  LineSize: 32;  Description: 'Unified L2 Cache, 256K, 8 ways, 32b lines'),
    (Descriptor: $83; Level: clLevel2;      Associativity: ca8Way;   Size: 512;  LineSize: 32;  Description: 'Unified L2 Cache, 512K, 8 ways, 32b lines'),
    (Descriptor: $84; Level: clLevel2;      Associativity: ca8Way;   Size: 1024; LineSize: 32;  Description: 'Unified L2 Cache, 1024K, 8 ways, 32b lines'),
    (Descriptor: $85; Level: clLevel2;      Associativity: ca8Way;   Size: 2048; LineSize: 32;  Description: 'Unified L2 Cache, 2048K, 8 ways, 32b lines'),
    (Descriptor: $86; Level: clLevel2;      Associativity: ca4Way;   Size: 512;  LineSize: 64;  Description: 'Unified L2 Cache, 512 KB, 4 ways, 64 byte lines'),
    (Descriptor: $87; Level: clLevel2;      Associativity: ca8Way;   Size: 1024; LineSize: 64;  Description: 'Unified L2 Cache, 1024 KB, 8 ways, 64 byte lines'),
    (Descriptor: $88; Level: clLevel3;      Associativity: ca4Way;   Size: 2048; LineSize: 64;  Description: 'Unified L3 Cache, 2048 KB, 4 ways, 64 byte lines (IA-64)'),
    (Descriptor: $89; Level: clLevel3;      Associativity: ca4Way;   Size: 4069; LineSize: 64;  Description: 'Unified L3 Cache, 4096 KB, 4 ways, 64 byte lines (IA-64)'),
    (Descriptor: $8A; Level: clLevel3;      Associativity: ca4Way;   Size: 8192; LineSize: 64;  Description: 'Unified L3 Cache, 8192 KB, 4 ways, 64 byte lines (IA-64)'),
    (Descriptor: $8D; Level: clLevel3;      Associativity: ca12Way;  Size: 3096; LineSize: 128; Description: 'Unified L3 Cache, 3096 KB, 12 ways, 128 byte lines (IA-64)'),
    (Descriptor: $90; Level: clCodeTLB;     Associativity: caFull;   Size: 4096; LineSize: 64;  Description: 'Code TLB, 4K...256M pages, fully, 64 entries (IA-64)'),
    (Descriptor: $96; Level: clDataTLB;     Associativity: caFull;   Size: 4;    LineSize: 32;  Description: 'Data TLB, 4K...256M pages, fully, 32 entries (IA-64)'),
    (Descriptor: $96; Level: clLevel1Data;  Associativity: caFull;   Size: 4;    LineSize: 32;  Description: 'Data L1 Cache, 4K...256M pages, fully, 32 entries (IA-64)'),
    (Descriptor: $9B; Level: clDataTLB;     Associativity: caFull;   Size: 4;    LineSize: 96;  Description: 'Data L2 TLB, 4K...256M pages, fully, 96 entries (IA-64)'),
    (Descriptor: $9B; Level: clLevel2;      Associativity: caFull;   Size: 4096; LineSize: 96;  Description: 'Data L2 Cache, 4K...256M pages, fully, 96 entries (IA-64)'),
    (Descriptor: $B0; Level: clCodeTLB;     Associativity: ca4Way;   Size: 4;    LineSize: 128; Description: 'Code TLB, 4K pages, 4 ways, 128 entries'),
    (Descriptor: $B1; Level: clCodeTLB;     Associativity: ca4way;   Size: 4096; LineSize: 4;   Description: 'Code TLB, 4M pages, 4 ways, 4 entries'),
    (Descriptor: $B2; Level: clCodeTLB;     Associativity: ca4way;   Size: 4096; LineSize: 64;  Description: 'Code TLB: 4KByte pages, 4-way set associative, 64 entries'),
    (Descriptor: $B3; Level: clDataTLB;     Associativity: ca4Way;   Size: 4;    LineSize: 128; Description: 'Data TLB: 4 KByte pages, 4-way set associative, 128 entries'),
    (Descriptor: $B4; Level: clDataTLB;     Associativity: ca4Way;   Size: 4;    LineSize: 256; Description: 'Data TLB1: 4 KByte pages, 4-way associative, 256 entries'),
    (Descriptor: $BA; Level: clDataTLB;     Associativity: ca4Way;   Size: 4;    LineSize: 64;  Description: 'Data TLB1: 4 KByte pages, 4-way associative, 64 entries'),
    (Descriptor: $C0; Level: clDataTLB;     Associativity: ca4Way;   Size: 4096; LineSize: 8;   Description: 'Data TLB: 4 KByte and 4 MByte pages, 4-way associative, 8 entries'),
    (Descriptor: $CA; Level: clDataTLB;     Associativity: ca4Way;   Size: 4096; LineSize: 512; Description: 'Shared 2nd-Level TLB: 4 KByte pages, 4-way associative, 512 entries'),
    (Descriptor: $D0; Level: clLevel3;      Associativity: ca4Way;   Size: 524288;  LineSize: 64;  Description: 'L3 Cache: 512 KByte, 4-way set associative, 64 byte line size'),
    (Descriptor: $D1; Level: clLevel3;      Associativity: ca4Way;   Size: 1048576;  LineSize: 64;  Description: 'L3 Cache: 1 MByte, 4-way set associative, 64 byte line size'),
    (Descriptor: $D2; Level: clLevel3;      Associativity: ca4Way;   Size: 2097152;  LineSize: 64;  Description: 'L3 Cache: 2 MByte, 4-way set associative, 64 byte line size'),
    (Descriptor: $D6; Level: clLevel3;      Associativity: ca8Way;   Size: 1048576;  LineSize: 64;  Description: 'L3 Cache: 1 MByte, 8-way set associative, 64 byte line size'),
    (Descriptor: $D7; Level: clLevel3;      Associativity: ca8Way;   Size: 2097152;  LineSize: 64;  Description: 'L3 Cache: 2 MByte, 8-way set associative, 64 byte line size'),
    (Descriptor: $D8; Level: clLevel3;      Associativity: ca8Way;   Size: 4194304;  LineSize: 64;  Description: 'L3 Cache: 4 MByte, 8-way set associative, 64 byte line size'),
    (Descriptor: $DC; Level: clLevel3;      Associativity: ca12Way;   Size: 1572864;  LineSize: 64;  Description: 'L3 Cache: 1.5 MByte, 12-way set associative, 64 byte line size'),
    (Descriptor: $DD; Level: clLevel3;      Associativity: ca12Way;   Size: 3145728;  LineSize: 64;  Description: 'L3 Cache: 3 MByte, 12-way set associative, 64 byte line size'),
    (Descriptor: $DE; Level: clLevel3;      Associativity: ca12Way;   Size: 6291456;  LineSize: 64;  Description: 'L3 Cache: 6 MByte, 12-way set associative, 64 byte line size'),
    (Descriptor: $E2; Level: clLevel3;      Associativity: ca16Way;   Size: 2097152;  LineSize: 64;  Description: 'L3 Cache: 2 MByte, 16-way set associative, 64 byte line size'),
    (Descriptor: $E3; Level: clLevel3;      Associativity: ca16Way;   Size: 4194304;  LineSize: 64;  Description: 'L3 Cache: 4 MByte, 16-way set associative, 64 byte line size'),
    (Descriptor: $E4; Level: clLevel3;      Associativity: ca16Way;   Size: 8388608;  LineSize: 64;  Description: 'L3 Cache: 8 MByte, 16-way set associative, 64 byte line size'),
    (Descriptor: $EA; Level: clLevel3;      Associativity: ca24Way;   Size: 12582912;  LineSize: 64;  Description: 'L3 Cache: 12MByte, 24-way set associative, 64 byte line size'),
    (Descriptor: $EB; Level: clLevel3;      Associativity: ca24Way;   Size: 18874368;  LineSize: 64;  Description: 'L3 Cache: 18MByte, 24-way set associative, 64 byte line size'),
    (Descriptor: $EC; Level: clLevel3;      Associativity: ca24Way;   Size: 25165824;  LineSize: 64;  Description: 'L3 Cache: 24MByte, 24-way set associative, 64 byte line size')
  );

  cVendorNames: array[cvNone..cvTransmeta] of TCpuVendorInfo = (
    (Signature: ''; Prefix: '';   Name: '';
      FeatureAvailability: faCommon; CacheDetect: vcdStandard),
    (Signature: 'BadCpuVendor'; Prefix: 'Unknown';   Name: 'Unknown Vendor';
      FeatureAvailability: faCommon; CacheDetect: vcdStandard),
    (Signature: 'GenuineIntel'; Prefix: 'Intel';     Name: 'Intel Corporation';
      FeatureAvailability: faIntel;  CacheDetect: vcdStandard),
    (Signature: 'AuthenticAMD'; Prefix: 'AMD';       Name: 'Advanced Micro Devices';
      FeatureAvailability: faAmd;    CacheDetect: vcdExtended),
    (Signature: 'CyrixInstead'; Prefix: 'Cyrix';     Name: 'Via Technologies Inc';
      FeatureAvailability: faCyrix;  CacheDetect: vcdCombined),
    (Signature: 'CentaurHauls'; Prefix: 'Via';       Name: 'Via Technologies Inc';
      FeatureAvailability: faCommon; CacheDetect: vcdExtended),
    (Signature: 'NexGenDriven'; Prefix: 'NexGen';    Name: 'NexGen Inc';
      FeatureAvailability: faCommon; CacheDetect: vcdStandard),
    (Signature: 'UMC UMC UMC '; Prefix: 'UMC';       Name: 'United Microelectronics Corp';
      FeatureAvailability: faCommon; CacheDetect: vcdStandard),
    (Signature: 'RiseRiseRise'; Prefix: 'Rise';      Name: 'Rise Technology';
      FeatureAvailability: faCommon; CacheDetect: vcdStandard),
    (Signature: 'SiS SiS SiS'; Prefix: 'SiS';      Name: 'SiS';
      FeatureAvailability: faCommon; CacheDetect: vcdStandard),
    (Signature: 'Geode by NSC'; Prefix: 'NSC';      Name: 'National Semiconductor';
      FeatureAvailability: faCommon; CacheDetect: vcdStandard),
    (Signature: 'GenuineTMx86'; Prefix: 'Transmeta'; Name: 'Transmeta';
      FeatureAvailability: faAmd;    CacheDetect: vcdExtended)
  );

type
  TCacheDetails = record
    Typ: Byte;
    Desc: string;
    Associativity: TCacheAssociativity;
    Ways,
    Partitions,
    Size,
    Shared,
    LineSize,
    Level: Cardinal;
    Descriptors: TCacheDescriptors;
  end;

  TCPUCacheDetails = class(TPersistent)
  private
    FLevel, FShared, FLineSize, FSize, FWays, FParts: Integer;
    FAssociativity: TCacheAssociativity;
    FDescriptors: TCacheDescriptors;
    FTyp: Byte;
    FDesc: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetContent(AContent: TCacheDetails);
    procedure Clear;

    property Descriptors: TCacheDescriptors read FDescriptors;
  published
    property Typ: Byte read FTyp stored False;
    property Descriptor: string read FDesc stored False;
    property Associativity: TCacheAssociativity read FAssociativity stored False;
    property LineSize: integer read FLineSize stored False;
    property Size: integer read FSize stored False;
    property Ways: integer read FWays stored False;
    property Partitions: integer read FParts stored False;
    property Level: integer read FLevel stored False;
    property SharedWays: integer read FShared stored False;
  end;

  TCPUSegmentedCache = class(TPersistent)
  private
    FData: TCPUCacheDetails;
    FUnified: TCPUCacheDetails;
    FCode: TCPUCacheDetails;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure SetContent(ACode, AData, AUnified: TCacheDetails);
  published
    property Code: TCPUCacheDetails read FCode stored False;
    property Data: TCPUCacheDetails read FData stored False;
    property Unified: TCPUCacheDetails read FUnified stored False;
  end;

  TCPUCache = class(TPersistent)
  private
    FLevel3: TCPUCacheDetails;
    FLevel2: TCPUCacheDetails;
    FLevel1: TCPUSegmentedCache;
    FTrace: TCPUCacheDetails;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddData(AType: TProcessorCacheType; ALevel,AAssoc: Byte; ALineSize: Word; ASize: Cardinal);
    procedure SetContent(ALevel1Code, ALevel1data, ALevel1Unified, ALevel2, ALevel3, ATrace: TCacheDetails);
  published
    property Level1: TCPUSegmentedCache read FLevel1 stored False;
    property Level2: TCPUCacheDetails read FLevel2 stored False;
    property Level3: TCPUCacheDetails read FLevel3 stored False;
    property Trace: TCPUCacheDetails read FTrace stored False;
  end;

  TCPUFeatureSet = class(TPersistent)
  private
    FAF: TAvailableFeatures;
    function GetCount: Cardinal;
    function GetFeature(Index: Byte): TCPUFeature;
    function GetFeatureByFlag(Flag: Byte): TCPUFeature;
  public
    destructor Destroy; override;

    procedure SetContent(AAF: TAvailableFeatures);

    property Features[Index: Byte]: TCPUFeature read GetFeature;
    property FeaturesByFlag[Flag: Byte]: TCPUFeature read GetFeatureByFlag;
  published
    property Count: Cardinal read GetCount;
  end;

  TCPUFeatures = class(TPersistent)
  private
    FStd1,FStd2,FExt1,FExt2,FAPM: TCPUFeatureSet;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetContent(AStd1,AStd2,AExt1,AExt2,AAPM: TAvailableFeatures);
  published
    property Standard1: TCPUFeatureSet read FStd1;
    property Standard2: TCPUFeatureSet read FStd2;
    property Extended1: TCPUFeatureSet read FExt1;
    property Extended2: TCPUFeatureSet read FExt2;
    property PowerManagement: TCPUFeatureSet read FAPM;
  end;

  {$IFDEF RAD9PLUS} [ComponentPlatformsAttribute(pidWin32 or pidWin64)] {$ENDIF}
  TMiTeC_CPU = class(TMiTeC_Component)
  private
    FCount: Byte;
    FPC: Byte;
    FfsStd,FfsExt,FfsAPM: TCPUIDResult;
    FCPUType: integer;
    FBrand: integer;
    FFamily: integer;
    FStepping: integer;
    FModel: integer;
    FVendor: TCPUVendor;
    FGenericName: string;
    FCPUIDSupported: boolean;
    FMarketingName: string;
    FCacheDescriptors: TCacheDescriptors;
    FCPUCache: TCPUCache;
    FCodeName: string;
    FCPUName: string;
    FFamilyEx: integer;
    FModelEx: integer;
    FSteppingEx: integer;
    FCPUFeatures: TCPUFeatures;
    FRevision: string;
    FTech: string;
    FFreq: Cardinal;
    FFDIVBug: Boolean;
    FIndex: Byte;
    FSN: string;
    FAPICID: Byte;
    FLPP: Byte;
    FLID: Byte;
    FPID: Byte;
    FTC,FCC: Byte;
    FIntelBrand: TIntelBrands;
    FLPC: Byte;
    FCPP: Byte;
    FArch: Word;
    FSI: TSystemInfo;
    FHTT: boolean;
    FMCPP,FMLPP, FMLPC: Cardinal;
    function ValidDescriptor(Value: Cardinal): Boolean;
    procedure DecodeDescriptor(Value: Cardinal; Index: Integer);
    function DescriptorExists(Value: Cardinal): Boolean;
    function DecodeCacheParams(ACACHE: TCPUIDResult): TCacheDetails;
    function LookupAssociativity(Value: Byte): TCacheAssociativity;
    procedure IntelLookupName;
    procedure AMDLookupName;
    procedure CyrixLookupName;
    procedure IDTLookupName;
    procedure NexGenLookupName;
    procedure UMCLookupName;
    procedure RiseLookupName;
    procedure SiSLookupName;
    procedure GeodeLookupName;
    procedure TransmetaLookupName;
    {procedure IntelLookupCode;
    procedure AMDLookupCode;}
    function GetCount: Byte;
    procedure SetIndex(const Value: Byte);
    function GetIntelBrand: TIntelBrands;
    function GetPhysCount: Byte;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure GetAvailableFeatures(AFLevel: TFeatureSet; var AF: TAvailableFeatures);
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;

    property MaxCorePerPackage: Cardinal read FMCPP stored False;
    property MaxLogicalPerPackage: Cardinal read FMLPP stored False;
    property MaxLogicalPerCore: Cardinal read FMLPC stored False;
  published
    property Architecture: Word read FArch stored False;
    property CPUCount: Byte read FCount stored False;
    property CPUPhysicalCount: Byte read FPC stored False;
    property Cache: TCPUCache read FCPUCache stored False;
    property Features: TCPUFeatures read FCPUFeatures stored False;
    property CPUIndex: Byte read FIndex write SetIndex stored False;
    property CPUIDSupported: boolean read FCPUIDSupported stored False;
    property Vendor: TCPUVendor read FVendor stored False;
    property CPUType: integer read FCPUType stored False;
    property Family: integer read FFamily stored False;
    property Model: integer read FModel stored False;
    property Stepping: integer read FStepping stored False;
    property FamilyEx: integer read FFamilyEx stored False;
    property ModelEx: integer read FModelEx stored False;
    property SteppingEx: integer read FSteppingEx stored False;
    property Brand: integer read FBrand stored False;
    property IntelBrand: TIntelBrands read FIntelBrand stored False;
    property GenericName: string read FGenericName stored False;
    property MarketingName: string read FMarketingName stored False;
    property CPUName: string read FCPUName stored False;
    property CodeName: string read FCodeName stored False;
    property Revision: string read FRevision stored False;
    property Technology: string read FTech stored False;
    property Frequency: Cardinal read FFreq stored False;
    property FDIVBug: Boolean read FFDIVBug stored False;
    property SerialNumber: string read FSN stored False;
    property APICID: Byte read FAPICID stored False;
    property LogicalPerPackage: Byte read FLPP stored False;
    property CorePerPackage: Byte read FCPP stored False;
    property LogicalPerCore: Byte read FLPC stored False;
    property ThreadCount: Byte read FTC stored False;
    property CoreCount: Byte read FCC stored False;
    property PhysicalID: Byte read FPID stored False;
    property LogicalID: Byte read FLID stored False;
  end;

type
  TFreqInfo = record
    RawFreq: Cardinal;
    NormFreq: Cardinal;
    DelayTime: Cardinal;
    InCycles: Cardinal;
    ExTicks: Cardinal;
  end;

function GetCPUSpeed: TFreqInfo;

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry, System.Math,
     {$ELSE}
     Registry, Math,
     {$ENDIF}
     MiTeC_NativeAPI, MiTeC_NativeDefs, MiTeC_StrUtils, MiTeC_RegUtils;

var
  OldAffinity: {$IFDEF NATIVEINT}NativeUInt{$ELSE}Cardinal{$ENDIF};

function GetNTCPUCount: Byte;
var
  sbi: TSystemBasicInformation;
  r: NTSTATUS;
begin
  r:=NtQuerySystemInformation(SystemBasicInformation,@sbi,SizeOf(sbi),nil);
  if r<>STATUS_SUCCESS then
    Result:=1
  else
    Result:=sbi.NumberOfProcessors;
end;

function FormatCPUName(const AName: string): string;
var
  i: Integer;
begin
  Result:=AName;
  Result:=StringReplace(Result,'(R)','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'(TM)',' ',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Genuine','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Procesor','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Processor','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Technology','',[rfReplaceAll,rfIgnoreCase]);
  i:=Pos('@',Result);
  if i>0 then begin
    Delete(Result,i,255);
    Result:=StringReplace(Result,'CPU','',[rfReplaceAll,rfIgnoreCase]);
  end else begin
    i:=Pos('CPU',Result);
    if i>0 then
       Delete(Result,i,255);
  end;
  Result:=Trim(Result);
  Result:=StripSpaces(Result);
end;

{$IFDEF FPC}
{$ASMMODE intel}
function ReadTimeStampCounter: Int64;
begin
{$ELSE}
function ReadTimeStampCounter: Int64; assembler;
{$ENDIF}
asm
  DW      $310F
{$IFDEF WIN64}
  SHL     RDX, 32
  OR      RAX, RDX
{$ENDIF}
{$IFDEF FPC}
end;
{$ENDIF}
end;

function RoundFrequency(Frequency: Integer): Integer;
const
  NF: array [0..9] of Integer = (0, 20, 33, 50, 60, 66, 80, 90, 100, 133);
var
  Freq, RF: Integer;
  i: Byte;
  Hi, Lo: Byte;
begin
  RF:=0;
  Freq:=Frequency mod 100;
  for i:=0 to High(NF) do begin
    if Freq<NF[i] then begin
      Hi:=i;
      Lo:=i-1;
      if (NF[Hi]-Freq)>(Freq-NF[Lo]) then
        RF:=NF[Lo]-Freq
      else
        RF:=NF[Hi]-Freq;
      Break;
    end;
  end;
  Result:=Frequency+RF;
end;

function GetCPUSpeed;
var
  T0, T1: Int64;
  CountFreq: Int64;
  Freq, Freq2, Freq3, Total: Int64;
  TotalCycles, Cycles: Int64;
  Stamp0, Stamp1: Int64;
  TotalTicks, Ticks: Double;
  Tries, Priority: Integer;
  Thread: THandle;
begin
  Stamp0:=0;
  Stamp1:=0;
  Freq :=0;
  Freq2:=0;
  Freq3:=0;
  Tries:=0;
  TotalCycles:=0;
  TotalTicks:=0;
  Total:=0;

  Thread:=GetCurrentThread();
  CountFreq:=0;
  if QueryPerformanceFrequency(CountFreq) then begin
    while ((Tries<3) or ((Tries<20) and ((Abs(3*Freq-Total)>3) or
      (Abs(3*Freq2-Total)>3) or (Abs(3*Freq3-Total)>3)))) do begin
      Inc(Tries);
      Freq3:=Freq2;
      Freq2:=Freq;
      T0:=0;
      QueryPerformanceCounter(T0);
      T1:=T0;
      Priority:=GetThreadPriority(Thread);
      if Priority <> THREAD_PRIORITY_ERROR_RETURN then
        SetThreadPriority(Thread, THREAD_PRIORITY_TIME_CRITICAL);
      try
        while T1 - T0 < 50 do begin
          QueryPerformanceCounter(T1);
          Stamp0:=ReadTimeStampCounter;
        end;
        T0:=T1;

        while T1 - T0 < 1000 do begin
          QueryPerformanceCounter(T1);
          Stamp1:=ReadTimeStampCounter;
        end;
      finally
        if Priority <> THREAD_PRIORITY_ERROR_RETURN then
          SetThreadPriority(Thread, Priority);
      end;

      Cycles:=Stamp1 - Stamp0;
      Ticks:=T1 - T0;
      Ticks:=Ticks * 100000;

      if CountFreq = 0 then
        Ticks:=High(Int64)
      else
        Ticks:=Ticks / (CountFreq / 10);

      TotalTicks:=TotalTicks + Ticks;
      TotalCycles:=TotalCycles + Cycles;

      if IsZero(Ticks) then
        Freq:=High(Freq)
      else
        Freq:=Round(Cycles / Ticks);

      Total:=Freq + Freq2 + Freq3;
    end;

    if IsZero(TotalTicks) then begin
      Freq3:=High(Freq3);
      Freq2:=High(Freq2);
      Result.RawFreq:=High(Result.RawFreq);
    end else  begin
      Freq3:=Round((TotalCycles *  10) / TotalTicks);
      Freq2:=Round((TotalCycles * 100) / TotalTicks);
      Result.RawFreq:=Round(TotalCycles / TotalTicks);
    end;

    Result.NormFreq:=Result.RawFreq;

    if Freq2 - (Freq3 * 10) >= 6 then
      Inc(Freq3);


    Freq:=Result.RawFreq * 10;
    if (Freq3 - Freq) >= 6 then
      Inc(Result.NormFreq);

    Result.ExTicks:=Round(TotalTicks);
    Result.InCycles:=TotalCycles;

    Result.NormFreq:=RoundFrequency(Result.NormFreq);
  end;
end;

function FormatString(AValue: Cardinal): string;
begin
  Result:=string(AnsiChar(LoByte(LoWord(AValue)))+
          AnsiChar(HiByte(LoWord(AValue)))+
          AnsiChar(LoByte(HiWord(AValue)))+
          AnsiChar(HiByte(HiWord(AValue))));
end;

function SplitToNibble(ANumber: String): String;
begin
  Result:=Copy(ANumber,0,4)+'-'+Copy(ANumber,5,4);
end;

function FDIVBugPresent: Boolean;
const
  N1: Real = 4195835.0;
  N2: Real = 3145727.0;
begin
  Result:=((((N1/N2)*N2)-N1)<>0.0);
end;

procedure SetProcAffinity;
var
  a,sa,ph: {$IFDEF NATIVEINT}NativeUInt{$ELSE}Cardinal{$ENDIF};
  si: TSystemInfo;
begin
  if FIndex in [0..31] then begin
    ph:=GetCurrentProcess;
    GetProcessAffinityMask(GetCurrentProcess,OldAffinity,sa);
    GetNativeSystemInfo(si);
    if FIndex>si.dwNumberOfProcessors-1 then
      FIndex:=0;
    a:=1 shl FIndex;
    if OldAffinity<>a then begin
      if Assigned(SetProcessAffinityMask) then
        SetProcessAffinityMask(ph,a);
      Sleep(0);
    end else
      OldAffinity:=uint(-1);
  end;
end;

procedure RestoreProcAffinity;
begin
  if Assigned(SetProcessAffinityMask) and (OldAffinity<>uint(-1)) then
    SetProcessAffinityMask(GetCurrentProcess,OldAffinity);
end;

{$IFDEF FPC}{$ASMMODE intel}{$ENDIF}
procedure CallCPUID(inEAX, inECX: Cardinal; out outEAX, outEBX, outECX, outEDX);
{$if not defined(WIN64) or defined(FPC)}
begin
{$ifend}
asm
{$IFDEF WIN32}
      PUSH    EDI
      PUSH    EBX
      MOV     EAX, inEAX
      MOV     ECX, inECX
      DW      CPUID_OpCode
      MOV     EDI, outEAX
      MOV     Cardinal PTR [EDI], EAX
      MOV     EAX, outEBX
      MOV     EDI, outECX
      MOV     Cardinal PTR [EAX], EBX
      MOV     Cardinal PTR [EDI], ECX
      MOV     EAX, outEDX
      MOV     Cardinal PTR [EAX], EDX
      POP  EBX
      POP  EDI
{$ENDIF WIN32}
{$IFDEF WIN64}
      PUSH    RBX
      MOV     EAX, inEAX
      MOV     ECX, inECX
      CPUID
      MOV     R8, outEAX
      MOV     R9, outEBX
      MOV     R10, outECX
      MOV     R11, outEDX
      MOV     Cardinal PTR [R8], EAX
      MOV     Cardinal PTR [R9], EBX
      MOV     Cardinal PTR [R10], ECX
      MOV     Cardinal PTR [R11], EDX
      POP     RBX
{$ENDIF}
end;
{$if not defined(WIN64) or defined(FPC)}
end;
{$ifend}

function GetCPUIDResult;
begin
  CallCPUID(CPUIDCommand,CPUIDLevel,Result.EAX,Result.EBX,Result.ECX,Result.EDX);
end;

{$IFDEF FPC}{$ASMMODE intel}{$ENDIF}
function ExecIsCPUIDSupported: Boolean;
{$if not defined(WIN64) or defined(FPC)}
begin
{$ifend}
asm
{$IFDEF WIN64}
    PUSHFQ
    POP     RAX
    MOV     RCX, RAX
    XOR     RAX, CPUID_EFlags
    AND     RCX, CPUID_EFlags
    PUSH    RAX
    POPFQ
    PUSHFQ
    POP     RAX
    AND     RAX, CPUID_EFlags
    XOR     RAX, RCX
    SETNZ   Result
{$ENDIF}
{$IFDEF WIN32}
    PUSHFD
    POP     EAX
    MOV     ECX, EAX
    XOR     EAX, CPUID_EFlags
    AND     ECX, CPUID_EFlags
    PUSH    EAX
    POPFD
    PUSHFD
    POP     EAX
    AND     EAX, CPUID_EFlags
    XOR     EAX, ECX
    SETNZ   Result
{$ENDIF}
{$if not defined(WIN64) or defined(FPC)}
end;
{$ifend}
end;

function IsCPUIDSupported(Cpu: Byte): Boolean;
begin
  SetProcAffinity(Cpu);
  try
    Result:=ExecIsCPUIDSupported;
  finally
    RestoreProcAffinity;
  end;
end;

function ExecCPUID(Command: Cardinal; Iterations: Integer): TCPUIDResult;
var
  i: Integer;
begin
  CPUIDCommand:=Command;
  for i:=0 to Iterations-1 do
    Result:=GetCPUIDResult;
end;

function ExecuteCPUID(Cpu: integer; Command: Cardinal; Iterations: Integer = 1; Level: Cardinal = 0): TCPUIDResult;
begin
  CPUIDCommand:=Command;
  CPUIDLevel:=Level;
  if CPU>-1 then
    SetProcAffinity(Cpu);
  try
    Result:=GetCPUIDResult;//ExecCPUID(Command,Iterations);
  finally
    if CPU>-1 then
      RestoreProcAffinity;
  end;
end;

function GetCPUIDCommandLevel(Command: Cardinal): TCPUIDExecutionLevel;
begin
  case Command of
    CPUID_STD_MaximumLevel..CPUID_STD_SerialNumber    : Result:=celStandard;
    CPUID_EXT_MaximumLevel..CPUID_EXT_AA64Information : Result:=celExtended;
    CPUID_TMX_MaximumLevel..CPUID_TMX_Operation       : Result:=celTransmeta;
  else
    Result := celStandard;
  end;
end;

function GetCPUIDMaximumCommand;
begin
  SetProcAffinity(Cpu);
  try
    case Level of
      celStandard:  Result:=ExecCPUID(CPUID_STD_MaximumLevel,1).EAX;
      celExtended:  Result:=ExecCPUID(CPUID_EXT_MaximumLevel,1).EAX;
      celTransmeta: Result:=ExecCPUID(CPUID_TMX_MaximumLevel,1).EAX;
    else
      Result := 0;
    end;
  finally
    RestoreProcAffinity;
  end;
end;

function IsCPUIDCommandSupported(Cpu: Byte; Command: Cardinal): Boolean;
begin
  Result:=Command<=GetCPUIDMaximumCommand(Cpu,GetCPUIDCommandLevel(Command));
end;

{ TMiTeC_CPU }

{procedure TMiTeC_CPU.AMDLookupCode;
const
  CoreArray: array[0..6] of record
                              Core: string;
                              Model: array[0..4] of string;
                            end = (
    (Core:'Lancaster';Model:('ML','MT','','','')),
    (Core:'Richmond';Model:('MK','','','','')),
    (Core:'Taylor';Model:('TL-50','','','','')),
    (Core:'Trinidad';Model:('TL-52','TL-56','TL-60','TL-64','')),
    (Core:'Tyler';Model:('TK-53','TL-58','TL-66','','')),
    (Core:'Winsdor';Model:('EE','','','','')),
    (Core:'Brisbane';Model:('BE-23','','','',''))
  );
var
  i,j: Integer;
begin
  if FCodeName<>'' then
    Exit;
  for i:=0 to High(CoreArray) do
    for j:=0 to High(CoreArray[i].Model) do
      if Pos(CoreArray[i].Model[j],FCPUName)>0 then begin
        FCodeName:=CoreArray[i].Core;
        Break;
      end;
end;}

procedure TMiTeC_CPU.AMDLookupName;
begin
  case FFamily of
    4: case FModel of
         0: FCPUName:='Am486DX';
         3,7: FCPUName:='Am486DX2';
         8,9: FCPUName:='Am486DX4';
         14,15: begin
           FCPUName:='Am5x86';
           FRevision:='A';
         end;
    end;
    5: case FModel of
         0: begin
           FCPUName:='K5';
           FCodename:='SSA/5';
           FTech:='0.50 µm';
           case FStepping of
             0: FRevision:='E';
             1: FRevision:='F';
           end;
         end;
         1,2,3: begin
           FCPUName:='K5';
           FCodename:='5k86';
           FTech:='0.35 µm';
         end;
         6: begin
           FCPUName:='K6';
           FCodename:='K6';
           FTech:='0.30 µm';
           case FStepping of
             1: FRevision:='B';
             2: FRevision:='C';
           end;
         end;
         7: begin
           FCPUName:='K6';
           FCodename:='Little Foot';
           FTech:='0.25 µm';
           FRevision:='A';
         end;
         8: begin
           FCPUName:='K6-II';
           FCodename:='Chomper';
           FTech:='0.25 µm';
           if FStepping=0 then
             FRevision:='A';
         end;
         9: begin
           FCPUName:='K6-III';
           FCodename:='Sharptooth';
           FTech:='0.25 µm';
           if FStepping=0 then
             FRevision:='A';
         end;
         13: FCPUName:='K6-II+/K6-III+';
    end;
    6: case FModel of
         0: begin
           FCPUName:='Athlon';
           FCodename:='Argon';
           FTech:='0.25 µm';
         end;
         1: begin
           FCPUName:='Athlon';
           FCodename:='Argon';
           FTech:='0.25 µm';
           case FStepping of
             1: FRevision:='C1';
             2: FRevision:='C2';
           end;
         end;
         2: begin
           FCPUName:='Athlon';
           FCodename:='Pluto/Orion';
           FTech:='0.18 µm';
           case FStepping of
             1: FRevision:='A1';
             2: FRevision:='A2';
           end;
         end;
         3: begin
           FCPUName:='Duron';
           FCodename:='Spitfire';
           FTech:='0.18 µm';
           case FStepping of
             0: FRevision:='A0';
             1: FRevision:='A1,A2';
           end;
         end;
         4: begin
           FCPUName:='Athlon';
           FCodename:='Thunderbird';
           FTech:='0.18 µm';
           case FStepping of
             2: FRevision:='A4,A5,A6,A7';
             4: FRevision:='A9';
           end;
         end;
         6: begin
           FCPUName:='Athlon XP';
           FCodename:='Palomino/Corvette';
           FTech:='0.18 µm';
           case FStepping of
             0: FRevision:='A0';
             1: FRevision:='A2';
             2: FRevision:='A5';
           end;
         end;
         7: begin
           FCPUName:='Duron';
           FCodename:='Morgan/Camaro';
           FTech:='0.18 µm';
           case FStepping of
             0: FRevision:='A0';
             1: FRevision:='A1';
           end;
         end;
         8: begin
           if FCPUCache.Level2.Size>=256 then begin
             if FCPUFeatures.FExt1.GetFeatureByFlag(EFS_MP).Available then
               FCPUName:='Sempron'
             else
               FCPUName:='Athlon XP';
           end else
             FCPUName:='Duron';
           FCodename:='Thoroughbred';
           FTech:='0.13 µm';
           case FStepping of
             0: FRevision:='A0';
             1: FRevision:='B0';
           end;
         end;
         9: begin
           FCPUName:='Athlon';
           FCodename:='Appaloosa';
           FTech:='0.13 µm';
         end;
         10: begin
           FCPUName:='Sempron';
           FTech:='0.13 µm';
           if FCPUFeatures.FExt1.GetFeatureByFlag(EFS_MP).Available then
             FCodename:='Sempron'
           else
             if FCPUCache.Level2.Size=512 then
               FCodename:='Athlon'
             else
               FCodename:='Athlon XP';
           FRevision:='A2';
           if FCPUCache.Level2.Size=64 then
             FCodename:='Applebred'
           else
             if FCPUCache.Level2.Size=256 then
               FCodename:='Geode'
             else
               FCodename:='Barton';
         end;
    end;
    15: begin
      case FModel of
         3: begin
              FCPUName:='Athlon 64';
              FCodename:='Toledo';
              FTech:='90 nm';
         end;
         4: begin
              FCPUName:='Athlon 64';
              FCodename:='Clawhammer';
              FTech:='0.13 µm';
              case FBrand shr 5 of
                1: if FCPUCache.Level2.Size=512 then
                     FCodename:='Newcastle'
                   else
                     FCodename:='Hammer';
                9: FCPUName:='Athlon 64 FX';
              end;
              if (FModel=4) and (FStepping=8) then
                FRevision:='SH7-C0';
         end;
         5: begin
              FCPUName:='Opteron';
              FCodename:='Sledgehammer';
              FTech:='0.13 µm';
              case FBrand shr 5 of
                3: FRevision:=Format(' UP1-%d',[38+2*(swap(FBrand shl 3) shr 11)]);
                4: FRevision:=Format(' DP2-%d',[38+2*(swap(FBrand shl 3) shr 11)]);
                5: FRevision:=Format(' MP8-%d',[38+2*(swap(FBrand shl 3) shr 11)]);
              end;
              case FStepping of
                1: FRevision:='SH7-B3';
                8: FRevision:='SH7-C0';
              end;
         end;
         6: begin
           FCPUName:='';
           FCodename:='Toledo';
           FTech:='90 nm';
         end;
         7: begin
           FCPUName:='';
           FCodename:='San Diego';
           FTech:='90 nm';
         end;
         8
         : begin
           FCPUName:='';
           FCodename:='Paris';
           FTech:='130 nm';
           if Hi(FSI.wProcessorRevision)=$48 then begin
             FCodename:='Taylor';
             FTech:='90 nm';
           end;
         end;
         9: begin
           FCPUName:='';
           FCodename:='K9';
           FTech:='90 nm';
         end;
         10: begin
           FCPUName:='';
           FCodename:='Victoria';
           FTech:='90 nm';
         end;
         11: begin
           FCPUName:='';
           case FModelEx of
             $4B: FCodename:='Windsor';
             11:  FCodename:='Windsor';
           end;
           FTech:='90 nm';
         end;
         12: begin
           FCPUName:='';
           FCodename:='Victoria';
           FTech:='90 nm';
         end;
         17: begin
           FCPUName:='';
           case FModelEx of
             $1F: FCodename:='Winchester';
             $2F: FCodename:='Venice';
           end;
           FTech:='90 nm';
         end;
       end;
       if (FCPP>1) and (FCPUName<>'') then
         FCPUName:=FCPUName+' X2';
       if (Pos('64',FCPUName)=0) and (Architecture=PROCESSOR_ARCHITECTURE_AMD64) then
         FCPUName:=FCPUName+' 64';
    end;
  end;
end;

procedure TMiTeC_CPU.Clear;
begin
  FIntelBrand:=[];
  FFamily:=0;
  FModel:=0;
  FStepping:=0;
  FFamilyEx:=0;
  FModelEx:=0;
  FSteppingEx:=0;
  FHTT:=False;
  FCPUName:='';
  FCodename:='';
  FRevision:='';
  FTech:='';
end;

constructor TMiTeC_CPU.Create;
begin
  inherited Create(AOwner);
  FCount:=1;
  FPC:=1;
  FIndex:=0;
  FCPUCache:=TCPUCache.Create;
  FCPUFeatures:=TCPUFeatures.Create;
  GetNativeSystemInfo(FSI);
  FArch:=FSI.wProcessorArchitecture;
  FCount:=FSI.dwNumberOfProcessors;
  FPC:=GetPhysCount;
end;

procedure TMiTeC_CPU.CyrixLookupName;
begin
  case FFamily of
    0: case FStepping of
         5: begin
           FCPUName:='Cx486S/D';
           FCodeName:='M5';
         end;
         6: begin
           FCPUName:='Cx486DX';
           FCodeName:='M6';
         end;
         7: begin
           FCPUName:='Cx486DX2';
           FCodeName:='M7';
         end;
         8: begin
           FCPUName:='Cx486DX4';
           FCodeName:='M8';
         end;
    end;
    4: case FModel of
         1: begin
           FCPUName:='Cx486SLC';
           FRevision:='A';
         end;
         2: begin
           FCPUName:='Cx5x86';
           if FStepping in [9,11,13,15] then
             FRevision:='0,rev1';
         end;
         4: begin
           FCPUName:='MediaGX';
           FRevision:='GX,GXm';
         end;
         9: begin
           FCPUName:='Cx5x86';
           FRevision:='0,rev2+';
           FTech:='0.65 µm';
         end;
       end;
    5: case FModel of
         2 :begin
           FCPUName:='6x86';
           FCodename:='M1';
           FTech:='0.65 µm';
         end;
         3 :begin
           FCPUName:='6x86L';
           FCodename:='M1L';
           FTech:='0.35 µm';
         end;
       end;
    6: case FModel of
         0: if FFreq<225 then begin
              FCPUName:='6x86MX';
              FCodeName:='M2';
              FTech:='0.35 µm';
            end else begin
              FCPUName:='M-II';
              FCodename:='M2';
              FTech:='0.35 µm';
            end;
         5: begin
           FCPUName:='VIA Cyrix III';
           FCodename:='Joshua';
         end;
         6: begin
           FCPUName:='VIA Cyrix 3';
           FCodename:='Samuel I';
           if FStepping=5 then
             FCodename:='Samuel II';
         end;
         7: begin
           FCPUName:='VIA Cyrix 3';
           FCodename:='Ezra';
         end;
       end;
  end;
end;

procedure TMiTeC_CPU.DecodeDescriptor(Value: Cardinal; Index: Integer);
begin
  if ValidDescriptor(Value) then begin
    FCacheDescriptors[Index*4-3]:=LoByte(LoWord(Value));
    FCacheDescriptors[Index*4-2]:=HiByte(LoWord(Value));
    FCacheDescriptors[Index*4-1]:=LoByte(HiWord(Value));
    FCacheDescriptors[Index*4]:=HiByte(HiWord(Value));
  end;
end;

function TMiTeC_CPU.DescriptorExists(Value: Cardinal): Boolean;
var
  i: Integer;
begin
  Result:=False;
  for i:=Low(FCacheDescriptors) to High(FCacheDescriptors) do
    if not Result then
      Result:=FCacheDescriptors[i]=Value;
end;

function TMiTeC_CPU.DecodeCacheParams;
var
  s: string;
begin
  s:='';
  ResetMemory(Result,SizeOf(Result));
  with Result do begin
    Level:=(ACACHE.EAX shr 5) and $7;
    Typ:=ACACHE.EAX and $1F;
    LineSize:=ACACHE.EBX and $FFF + 1;
    Ways:=ACACHE.EBX shr 22 + 1;
    Partitions:=(ACACHE.EBX shr 12) and $FF +1;
    Size:=LineSize*Ways*Partitions*(ACACHE.ECX+1) shr 10;
    Shared:=(ACACHE.EAX shr 14) and $FFF +1;
    if Shared<FLPC then
      Shared:=FLPC;
    if Shared>FMCPP then
      Shared:=1;
    case Typ of
      ctDataCache: s:='Data';
      ctCodecache: s:='Code';
      ctUnifiedCache: s:='Unified';
    end;
    Desc:=Format('L%d %s %d KB, %d-way set associative, %d-byte line size',[Level,s,Size,Ways,LineSize]);
  end;
end;

destructor TMiTeC_CPU.Destroy;
begin
  if Assigned(FCPUCache) then
    FCPUCache.Free;
  if Assigned(FCPUFeatures) then
    FCPUFeatures.Free;
  inherited;
end;

procedure TMiTeC_CPU.GeodeLookupName;
begin
  FCPUName:='Geode';
end;

procedure TMiTeC_CPU.GetAvailableFeatures;
var
  i,j: integer;
  idx: Integer;
  fs: TFeatureAvailability;
  cpuid: TCPUIDResult;
begin
  Finalize(AF);

  case FVendor of
    cvIntel: fs:=faIntel;
    cvAMD: fs:=faAMD;
    cvCyrix: fs:=faCyrix;
    else  fs:=faCommon;
  end;

  i:=0;
  repeat
    if (cFeatureDetails[i].Info=faCommon) and (AFLevel in cFeatureDetails[i].Level) then begin
      SetLength(AF,Length(AF)+1);
      with AF[High(AF)] do begin
        Index:=cFeatureDetails[i].Index;
        Mnemonic:=cFeatureDetails[i].Mnemonic;
        Name:=cFeatureDetails[i].Name;
        Level:=AFLevel;
        Available:=False;
      end;
    end;
    Inc(i);
  until (i>High(cFeatureDetails));

  i:=0;
  repeat
    if (cFeatureDetails[i].Info=fs) and (AFLevel in cFeatureDetails[i].Level) then begin
      idx:=-1;
      for j:=0 to High(AF) do
        if AF[j].Index=cFeatureDetails[i].Index then begin
          idx:=j;
          Break;
        end;
      if idx=-1 then begin
        SetLength(AF,Length(AF)+1);
        idx:=High(AF);
      end;
      with AF[idx] do begin
        Index:=cFeatureDetails[i].Index;
        Mnemonic:=cFeatureDetails[i].Mnemonic;
        Name:=cFeatureDetails[i].Name;
        Level:=AFLevel;
        Available:=False;
        // Amd and Cyrix/Via use different bits for extended MMX
        if SameText(Mnemonic,'MMX+') then begin
          if fs=faAmd then
            Index:=EFS_MMXAMD;
          if fs=faCyrix then
            Index:=EFS_MMXVIA;
        end;
        // Amd K5 (model 0) use bit 9 (APIC) to report PGE support
        if SameText(Mnemonic,'PGE') then
          if (fs=faAmd) and (FFamily=5) and (FModel=0) then
            Index:=SFS_APIC;
        // Amd K6 use reserved bit 10 to report SEP support
        if SameText(Mnemonic,'SEP') then
          if (fs=faAmd) and (FFamily=6) then
            Index:=SFS_SEPK6;
      end;
    end;
    Inc(i);
  until (i>High(cFeatureDetails));

  for i:=0 to High(AF) do begin
    case AF[i].Level of
      fsExtended1,fsExtended2: cpuid:=FfsEXT;
      fsPowerManagement: cpuid:=FfsAPM;
      else cpuid:=FfsSTD;
    end;
    if AF[i].Level in [fsStandard1,fsExtended1,fsPowerManagement] then
      AF[i].Available:=(cpuid.EDX and (1 shl AF[i].Index))<>0
    else
      AF[i].Available:=(cpuid.ECX and (1 shl AF[i].Index))<>0;
  end;
end;

function TMiTeC_CPU.GetCount: Byte;
var
  si: TSystemInfo;
begin
  GetNativeSystemInfo(si);
  Result:=si.dwNumberOfProcessors;
end;

procedure TMiTeC_CPU.RefreshData;
const
  rkCPU = {HKEY_LOCAL_MACHINE}'\HARDWARE\DESCRIPTION\System\CentralProcessor\%d';
  rvVendorID = 'VendorIdentifier';
  rvGenericName = 'Identifier';
  rvMarketingName = 'ProcessorNameString';
  rvFreq = '~MHz';
  rvFeatureSet = 'FeatureSet';
var
  FCPUID, FAMD, FCACHE: TCPUIDResult;
  i,c: integer;
  j: TCacheLevel;
  vid,s: string;
  cd1: array[clLevel1Code..clLevel1Unified] of TCacheDetails;
  cd,cd2,cd3,cd4: TCacheDetails;
  fs1,fs2,fs3,fs4,fs5: TAvailableFeatures;
  mask,shift,b,n: Cardinal;

  slpi: array of TSystemLogicalProcessorInformation;

  procedure SetCache;
  begin
    if (cd.Level=1) and (cd.Typ=1) then
      cd1[clLevel1Data]:=cd
    else if (cd.Level=1) and (cd.Typ=2) then
      cd1[clLevel1Code]:=cd
    else if (cd.Level=1) and (cd.Typ=3) then
      cd1[clLevel1Unified]:=cd
    else if (cd.Level=2) then
      cd2:=cd
    else if (cd.Level=3) then
      cd3:=cd;
  end;

begin
  inherited;
  Clear;

  FCount:=GetCount;
  FPC:=GetPhysCount;

  FCPUIDSupported:=IsCPUIDSupported(FIndex);
// CPU Vendor
  if FCPUIDSupported then begin
    FCPUID:=ExecuteCPUID(FIndex,CPUID_STD_VendorSignature,1);
    vid:=FormatString(FCPUID.EBX)+FormatString(FCPUID.EDX)+FormatString(FCPUID.ECX);
    FVendor:=cvUnknown;
    for i:=Integer(cvUnknown) to Integer(cvTransmeta) do
      if cVendorNames[TCPUVendor(i)].Signature=vid then begin
        FVendor:=TCPUvendor(i);
        Break;
      end;
// CPU Generic Info
    if IsCPUIDCommandSupported(FIndex,CPUID_STD_Signature) then begin
      FCPUID:=ExecuteCPUID(FIndex,CPUID_STD_Signature,1);
      FCPUType:=(FCPUID.EAX shr 12 and 3);
      if (FCPUID.EAX shr 8 and $F) >= $F then
        FFamily:=(FCPUID.EAX shr 20 and $FF) + (FCPUID.EAX shr 8 and $F)
      else
        FFamily:=FCPUID.EAX shr 8 and $F;
      if (FCPUID.EAX shr 4 and $F) >= $F then
        FModel:=(FCPUID.EAX shr 16 and $F) + (FCPUID.EAX shr 4 and $F)
      else
        FModel:=FCPUID.EAX shr 4 and $F;
      FStepping:=FCPUID.EAX and $F;
      if (FVendor=cvAmd) and (FFamily=15) and (FModel>4) and (FCPUID.EBX=0) then begin  // AMD Opteron
        FAMD:=ExecuteCPUID(FIndex,CPUID_EXT_Signature,1);
        FBrand:=LoByte(LoWord(FAMD.EBX));
      end else
        FBrand:=LoByte(LoWord(FCPUID.EBX));

      case FArch of
        PROCESSOR_ARCHITECTURE_IA64: FGenericName:=Format(rsGenericName_ia64,[SystemInfo.wProcessorLevel,Hi(SystemInfo.wProcessorRevision),Lo(SystemInfo.wProcessorRevision)]);
        PROCESSOR_ARCHITECTURE_AMD64: FGenericName:=Format(rsGenericName_x64,[SystemInfo.wProcessorLevel,Hi(SystemInfo.wProcessorRevision),Lo(SystemInfo.wProcessorRevision)]);
        else FGenericName:=Format(rsGenericName_x86,[FFamily,FModel,FStepping]);
      end;

// APIC, physical and logical ID
      FHTT:=(FCPUID.EDX and (1 shl SFS_HTT))<>0;
      if FHTT then begin
        FAPICID:=(FCPUID.EBX and INITIAL_APIC_ID_BITS) shr 24;
        FMLPP:=GetBitsFromDWORD(FCPUID.EBX,16,23);
      end else begin
        FAPICID:=Byte(-1);
        FMLPP:=1;
      end;
      b:=1;
      mask:=PHY_ID_MASK;
      shift:=PHY_ID_SHIFT;
      while (b<FMLPP) do begin
        b:=b*2;
        mask:=mask shl 1;
        Inc(shift);
      end;
      FLID:=FAPICID and not mask shl 24 shr 24;
      FPID:=FAPICID shr shift;

// Core number
      FCPP:=1;
      FLPP:=1;
      FLPC:=1;
      case FVendor of
        cvIntel: if IsCPUIDCommandSupported(FIndex,CPUID_STD_CacheParams) then begin
          FCPUID:=ExecuteCPUID(-1{FIndex},CPUID_STD_CacheParams);
          FMCPP:=GetBitsFromDWORD(FCPUID.EAX,26,31)+1;
          FMLPC:=FMLPP div FMCPP;
          if FMCPP=2 then
            FIntelBrand:=FIntelBrand+[ibDuoCore]
          else
            if (FFamily=15) then
              FIntelBrand:=FIntelBrand+[ibP4];
          if IsCPUIDCommandSupported(FIndex,CPUID_STD_Topology) then begin
            for i:=0 to 254 do begin
              FCPUID:=ExecuteCPUID(-1,CPUID_STD_Topology,1,i);
              if GetBitsFromDWORD(FCPUID.EBX,0,15)>0 then begin
                 case GetBitsFromDWORD(FCPUID.ECX,8,15) of
                   1: {SMT} FLPC:=GetBitsFromDWORD(FCPUID.EAX,0,4);
                   2: {Core} FCPP:=GetBitsFromDWORD(FCPUID.EAX,0,4);
                 end;
              end else
                Break;
            end;
          end;
          if FCPP=0 then
            FCPP:=FMCPP;
          FCPP:=FMCPP div FCPP;
          if FCPP=0 then
            FCPP:=FMCPP;
          if FMLPC>FLPC then
            FLPC:=FMLPC;
          FLPP:=FCPP*FLPC;

          if (FCount div FPC)>FLPP then begin
            FCPP:=(FCount div FPC) div FLPC;
            FLPP:=(FCount div FPC);
          end;
        end;
        cvAMD: if IsCPUIDCommandSupported(FIndex,CPUID_EXT_AA64Information) then begin
          FCPUID:=ExecuteCPUID(FIndex,CPUID_EXT_AA64Information);
          FCPP:=(FCPUID.ECX and $FF)+1;
        end;
      end;
// CPU Marketing Name
      if FVendor<>cvTransmeta then
        if IsCPUIDCommandSupported(FIndex,CPUID_EXT_MarketingName1) then begin
          FCPUID:=ExecuteCPUID(FIndex,CPUID_EXT_MarketingName1,1);
          FMarketingName:=FormatString(FCPUID.EAX)+FormatString(FCPUID.EBX)+FormatString(FCPUID.ECX)+FormatString(FCPUID.EDX);
          FCPUID:=ExecuteCPUID(FIndex,CPUID_EXT_MarketingName2,1);
          FMarketingName:=FMarketingName+FormatString(FCPUID.EAX)+FormatString(FCPUID.EBX)+FormatString(FCPUID.ECX)+FormatString(FCPUID.EDX);
          FCPUID:=ExecuteCPUID(FIndex,CPUID_EXT_MarketingName3,1);
          FMarketingName:=FMarketingName+FormatString(FCPUID.EAX)+FormatString(FCPUID.EBX)+FormatString(FCPUID.ECX)+FormatString(FCPUID.EDX);
        end
      else
        if IsCPUIDCommandSupported(FIndex,CPUID_TMX_MarketingName1) then begin
          FCPUID:=ExecuteCPUID(FIndex,CPUID_TMX_MarketingName1,1);
          FMarketingName:=FormatString(FCPUID.EAX)+FormatString(FCPUID.EBX)+FormatString(FCPUID.ECX)+FormatString(FCPUID.EDX);
          FCPUID:=ExecuteCPUID(FIndex,CPUID_TMX_MarketingName2,1);
          FMarketingName:=FMarketingName+FormatString(FCPUID.EAX)+FormatString(FCPUID.EBX)+FormatString(FCPUID.ECX)+FormatString(FCPUID.EDX);
          FCPUID:=ExecuteCPUID(FIndex,CPUID_TMX_MarketingName3,1);
          FMarketingName:=FMarketingName+FormatString(FCPUID.EAX)+FormatString(FCPUID.EBX)+FormatString(FCPUID.ECX)+FormatString(FCPUID.EDX);
          FCPUID:=ExecuteCPUID(FIndex,CPUID_TMX_MarketingName4,1);
          FMarketingName:=FMarketingName+FormatString(FCPUID.EAX)+FormatString(FCPUID.EBX)+FormatString(FCPUID.ECX)+FormatString(FCPUID.EDX);
        end;
        FMarketingName:=Trim(FMarketingName);
    end;
// CPU Ext Signature
   if IsCPUIDCommandSupported(FIndex,CPUID_EXT_Signature) then begin
     FCPUID:=ExecuteCPUID(FIndex,CPUID_EXT_Signature,1);
     FFamilyEx:=FCPUID.EAX shr 8 and $F;
     FModelEx:=FCPUID.EAX shr 4 and $F;
     FSteppingEx:=FCPUID.EAX and $F;
   end;
   FModelEx:=Hi(FSI.wProcessorRevision);
   FSteppingEx:=Lo(FSI.wProcessorRevision);

// CPU cache
   for j:=clLevel1Code to clLevel1Unified do begin
     ResetMemory(cd1[j],SizeOf(TCacheDetails));
     cd1[j].Shared:=1;
   end;
   ResetMemory(cd2,SizeOf(TCacheDetails));
   cd2.Shared:=1;
   ResetMemory(cd3,SizeOf(TCacheDetails));
   cd3.Shared:=1;
   ResetMemory(cd4,SizeOf(TCacheDetails));
   cd4.Shared:=1;
   for i:=0 to 9 do begin
     FCACHE:=ExecuteCPUID(FIndex,CPUID_STD_CacheParams,1,i);
     cd:=DecodeCacheParams(FCACHE);
     if cd.Typ=0 then
       Break;
     SetCache;
   end;

// CPU Cache Level1
    if cVendorNames[FVendor].CacheDetect<>vcdExtended then begin
      if FVendor=cvIntel then
        FCACHE:=ExecuteCPUID(FIndex,CPUID_STD_CacheTlbs,LoByte(LoWord(ExecuteCPUID(FIndex,CPUID_STD_CacheTlbs,1).EAX)))
      else
        FCACHE:=ExecuteCPUID(FIndex,CPUID_EXT_Level1Cache,1);
      DecodeDescriptor(FCACHE.EAX,1);
      DecodeDescriptor(FCACHE.EBX,2);
      DecodeDescriptor(FCACHE.ECX,3);
      DecodeDescriptor(FCACHE.EDX,4);
      for j:=clLevel1Code to clLevel1Unified do begin
        if cd1[j].Typ=0 then begin
          cd1[j].Descriptors:=FCacheDescriptors;
          for i:=Low(cDescriptorInfo) to High(cDescriptorInfo) do
            if (cDescriptorInfo[i].Level=j) and DescriptorExists(cDescriptorInfo[i].Descriptor) then begin
              cd1[j].Associativity:=cDescriptorInfo[i].Associativity;
              cd1[j].LineSize:=cDescriptorInfo[i].LineSize;
              cd1[j].Size:=cDescriptorInfo[i].Size;
              cd1[j].Typ:=cDescriptorInfo[i].Descriptor;
              cd1[j].Desc:=cDescriptorInfo[i].Description;
            end;
        end;
      end;
    end else begin
      if cd1[clLevel1Code].Typ=0 then begin
        FCACHE:=ExecuteCPUID(FIndex,CPUID_EXT_Level1Cache,1);
        cd1[clLevel1Code].Size:=HiByte(HiWord(FCACHE.EDX));
        cd1[clLevel1Code].LineSize:=LoByte(LoWord(FCACHE.EDX));
        cd1[clLevel1Code].Associativity:=LookupAssociativity(LoByte(HiWord(FCACHE.EDX)));
        cd1[clLevel1Code].Typ:=ctCodeCache;
        with cd1[clLevel1Code] do
          Desc:=Format('L1 Code %d KB, %s, %d-byte line size',[Size,cAssociativityDescription[Associativity],LineSize]);
      end;

      if cd1[clLevel1Data].Typ=0 then begin
        cd1[clLevel1Data].Size:=HiByte(HiWord(FCACHE.ECX));
        cd1[clLevel1Data].LineSize:=LoByte(LoWord(FCACHE.ECX));
        cd1[clLevel1Data].Associativity:=LookupAssociativity(LoByte(HiWord(FCACHE.ECX)));
        cd1[clLevel1Data].Typ:=ctDataCache;
        with cd1[clLevel1Data] do
          Desc:=Format('L1 Data %d KB, %s, %d-byte line size',[Size,cAssociativityDescription[Associativity],LineSize]);
      end;
    end;
// CPU Cache Level2
    if cd2.Typ=0 then begin
      if cVendorNames[FVendor].CacheDetect<>vcdExtended then begin
        if FVendor=cvIntel then
          FCACHE:=ExecuteCPUID(FIndex,CPUID_STD_CacheTlbs,LoByte(LoWord(ExecuteCPUID(FIndex,CPUID_STD_CacheTlbs,1).EAX)))
        else
          FCACHE:=ExecuteCPUID(FIndex,CPUID_EXT_Level2Cache,1);
        DecodeDescriptor(FCACHE.EAX,1);
        DecodeDescriptor(FCACHE.EBX,2);
        DecodeDescriptor(FCACHE.ECX,3);
        DecodeDescriptor(FCACHE.EDX,4);
        cd2.Descriptors:=FCacheDescriptors;
        for i:=Low(cDescriptorInfo) to High(cDescriptorInfo) do
          if cDescriptorInfo[i].Level=clLevel2 then begin
            if DescriptorExists(cDescriptorInfo[i].Descriptor) then begin
              if (cDescriptorInfo[i].Descriptor=$49) and not(ibDuoCore in FIntelBrand) then
                Continue;
              cd2.LineSize:=cDescriptorInfo[i].LineSize;
              cd2.Size:=cDescriptorInfo[i].Size;
              cd2.Typ:=cDescriptorInfo[i].Descriptor;
              cd2.Desc:=cDescriptorInfo[i].Description;
              cd2.Associativity:=cDescriptorInfo[i].Associativity;
            end;
          end;
      end else begin
        FCACHE:=ExecuteCPUID(FIndex,CPUID_EXT_Level2Cache,1);
        if FVendor=cvIDT then
          cd2.Size:=HiByte(HiWord(FCACHE.ECX))
        else
          cd2.Size:=HiWord(FCACHE.ECX);
        cd2.LineSize:=LoByte(LoWord(FCACHE.ECX));
        if (FVendor=cvAmd) and (FFamily=6) then
          cd2.Associativity:=ca16Way
        else
          cd2.Associativity:=LookupAssociativity(LoByte(HiWord(FCACHE.ECX))); // ?
        cd2.Typ:=ctUnifiedCache;
        with cd2 do
          Desc:=Format('L2 Unified %d KB, %s, %d-byte line size',[Size,cAssociativityDescription[Associativity],LineSize]);
      end;
    end;
// CPU Cache Level3
    if cd3.Typ=0 then begin
      if cVendorNames[FVendor].CacheDetect<>vcdExtended then begin
        if FVendor=cvIntel then
          FCACHE:=ExecuteCPUID(FIndex,CPUID_STD_CacheTlbs,LoByte(LoWord(ExecuteCPUID(FIndex,CPUID_STD_CacheTlbs,1).EAX)))
        else
          FCACHE:=ExecuteCPUID(FIndex,CPUID_EXT_Level2Cache,1);
        DecodeDescriptor(FCACHE.EAX,1);
        DecodeDescriptor(FCACHE.EBX,2);
        DecodeDescriptor(FCACHE.ECX,3);
        DecodeDescriptor(FCACHE.EDX,4);
        cd3.Descriptors:=FCacheDescriptors;
        for i:=Low(cDescriptorInfo) to High(cDescriptorInfo) do
          if cDescriptorInfo[i].Level=clLevel3 then begin
            if DescriptorExists(cDescriptorInfo[i].Descriptor) then begin
              if (cDescriptorInfo[i].Descriptor=$49) and not(ibP4 in FIntelBrand) then
                Continue;
              cd3.Associativity:=cDescriptorInfo[i].Associativity;
              cd3.LineSize:=cDescriptorInfo[i].LineSize;
              cd3.Size:=cDescriptorInfo[i].Size;
              cd3.Typ:=cDescriptorInfo[i].Descriptor;
              cd3.Desc:=cDescriptorInfo[i].Description;
            end;
          end;
      end;
    end;
// CPU Cache Trace
    if cVendorNames[FVendor].CacheDetect<>vcdExtended then begin
      if FVendor=cvIntel then
        FCACHE:=ExecuteCPUID(FIndex,CPUID_STD_CacheTlbs,LoByte(LoWord(ExecuteCPUID(FIndex,CPUID_STD_CacheTlbs,1).EAX)))
      else
        FCACHE:=ExecuteCPUID(FIndex,CPUID_EXT_Level2Cache,1);
      DecodeDescriptor(FCACHE.EAX,1);
      DecodeDescriptor(FCACHE.EBX,2);
      DecodeDescriptor(FCACHE.ECX,3);
      DecodeDescriptor(FCACHE.EDX,4);
      cd4.Descriptors:=FCacheDescriptors;
      for i:=Low(cDescriptorInfo) to High(cDescriptorInfo) do
        if cDescriptorInfo[i].Level=clTrace then begin
          if DescriptorExists(cDescriptorInfo[i].Descriptor) then begin
            cd4.Associativity:=cDescriptorInfo[i].Associativity;
            cd4.LineSize:=cDescriptorInfo[i].LineSize;
            cd4.Size:=cDescriptorInfo[i].Size;
            cd4.Typ:=cDescriptorInfo[i].Descriptor;
            cd4.Desc:=cDescriptorInfo[i].Description;
          end;
       end;
    end;
    FCPUCache.SetContent(cd1[clLevel1Code],cd1[clLevel1Data],cd1[clLevel1Unified],cd2,cd3,cd4);
// CPU Features
    Finalize(fs1);
    Finalize(fs2);
    Finalize(fs3);
    Finalize(fs4);
    Finalize(fs5);
    if IsCPUIDCommandSupported(FIndex,CPUID_STD_FeatureSet) then begin
      FfsStd:=ExecuteCPUID(FIndex,CPUID_STD_FeatureSet,1);
      GetAvailableFeatures(fsStandard1,fs1);
      GetAvailableFeatures(fsStandard2,fs2);
    end;
    if IsCPUIDCommandSupported(FIndex,CPUID_EXT_FeatureSet) then begin
      FfsExt:=ExecuteCPUID(FIndex,CPUID_EXT_FeatureSet,1);
      GetAvailableFeatures(fsExtended1,fs3);
      GetAvailableFeatures(fsExtended2,fs4);
    end;
    if IsCPUIDCommandSupported(FIndex,CPUID_EXT_PowerManagement) then begin
      FfsAPM:=ExecuteCPUID(FIndex,CPUID_EXT_PowerManagement,1);
      GetAvailableFeatures(fsPowerManagement,fs5);
    end;
    FCPUFeatures.SetContent(fs1,fs2,fs3,fs4,fs5);
// CPU Serial Number
    FCPUID:=ExecuteCPUID(FIndex,CPUID_STD_Signature,1);
    if FVendor<>cvTransmeta then
      FSN:=Format('%s-%s-%s',[SplitToNibble(IntToHex(FCPUID.EAX,8)),
                              SplitToNibble(IntToHex(FCPUID.EDX,8)),
                              SplitToNibble(IntToHex(FCPUID.ECX,8))])
    else
      FSN:=SplitToNibble(IntToHex(FCPUID.EBX,8));
  end;
// CPU Speed
  SetProcAffinity(FIndex);
  try
    FFreq:=GetCPUSpeed.NormFreq;
  finally
    RestoreProcAffinity;
  end;

  //if not CPUID_Complete then
    with OpenRegistryReadOnly do
      try
        Rootkey:=HKEY_LOCAL_MACHINE;
        if OpenKey(Format(rkCPU,[FIndex]),False) then begin
          if FVendor=cvUnknown then begin
            vid:=ReadString(rvVendorID);
            for i:=Integer(cvUnknown) to Integer(cvTransmeta) do
              if SameText(cVendorNames[TCPUVendor(i)].Signature,vid) then begin
                FVendor:=TCPUvendor(i);
                Break;
              end;
          end;
          try
            s:=ReadString(rvGenericName);
            if Trim(s)<>'' then
              FGenericName:=s;
          except end;
          if FMarketingName='' then
            try
              FMarketingName:=ReadString(rvMarketingName);
            except
            end;
          CloseKey;
        end;
      finally
        Free;
      end;

// CPU Name, Codename, Revision, Technology
  FIntelBrand:=GetIntelBrand;
  case FVendor of
    cvIntel    :
      if Trim(FMarketingName)<>'' then begin
        FCPUName:=StringReplace(FMarketingName,'Intel','',[rfReplaceAll,rfIgnoreCase]);
        FCPUName:=FormatCPUName(FCPUName);
        if ibMobile in FIntelBrand then
          FCPUName:='Mobile '+FCPUName;
      end else
        IntelLookupName;
    cvAmd      :
      if Trim(FMarketingName)<>'' then begin
        FCPUName:=StringReplace(FMarketingName,'AMD','',[rfReplaceAll,rfIgnoreCase]);
        FCPUName:=FormatCPUName(FCPUName);
      end else
        AMDLookupName;
    cvCyrix    : CyrixLookupName;
    cvIDT      : IDTLookupName;
    cvNexGen   : NexGenLookupName;
    cvUMC      : UMCLookupName;
    cvRise     : RiseLookupName;
    cvSiS      : SiSLookupName;
    cvGeode    : GeodeLookupName;
    cvTransmeta: TransmetaLookupName;
  end;

  FFDIVBug:=FDIVBugPresent;

  FCC:=CPUPhysicalCount*CorePerPackage;
  FTC:=GetNTCPUCount;
  FLPP:=FTC div CPUPhysicalCount;
  FLPC:=FTC div FCC;

  if Assigned(GetLogicalProcessorInformation) then
  begin
    n:=0;
    SetLength(slpi,1);
    if not GetLogicalProcessorInformation(@slpi[0],n) then begin
      if GetLastError=ERROR_INSUFFICIENT_BUFFER then begin
        SetLength(slpi,n div SizeOf(TSystemLogicalProcessorInformation)+1);
        if GetLogicalProcessorInformation(@slpi[0],n) then begin
          n:=0;
          c:=0;
          b:=0;
          for i:=0 to High(slpi)-1 do
            case slpi[i].Relationship of
              RelationProcessorCore: begin
                Inc(c);
                Inc(n,CountSetBits(slpi[i].ProcessorMask));
              end;
              RelationCache: Inc(b);
            end;
          if not Odd(c) and not Odd(n) and (b>1) then begin
            FCC:=0;
            FPC:=0;
            FTC:=0;
            FCPUCache.Clear;
            for i:=0 to High(slpi)-1 do
              case slpi[i].Relationship of
                RelationProcessorCore: begin
                  Inc(FCC);
                  Inc(FTC,CountSetBits(slpi[i].ProcessorMask));
                end;
                RelationProcessorPackage: Inc(FPC);
                RelationCache: with slpi[i].Cache do
                   FCPUCache.AddData(_Type,Level,Associativity,LineSize,Size shr 10);
              end;
            if FPC=0 then
              FPC:=1;
            if FCC=0 then
              FCC:=1;
            FLPP:=FTC div FPC;
            FCPP:=FCC div FPC;
            FLPC:=FTC div FCC;
          end;
        end;
      end;
    end;
  end;

  SetDataAvail(True);
end;

procedure TMiTeC_CPU.IDTLookupName;
begin
  case FFamily of
    5: case FModel of
         4: begin
           FCPUName:='WinChip';
           FCodename:='C6';
           FTech:='0.35 µm';
         end;
         8: begin
           FCPUName:='WinChip 2';
           FCodename:='C6-2';
           FTech:='0.35-0.25 µm';
           case FStepping of
             1,5: FRevision:='WC2';
             7..9: FRevision:='WC2A';
             10..15: FRevision:='WC2B';
           end;
         end;
         9: begin
           FCPUName:='WinChip 3';
           FTech:='0.25 µm';
         end;
       end;
    6: case FModel of
         6: begin
           FCPUName:='VIA C3 C5A';
           FCodename:='Samuel 1';
           FTech:='0.18 µm';
         end;
         7: if FStepping<8 then begin
              FCPUName:='VIA C3 C5B';
              FCodename:='Samuel 2';
              FTech:='0.13 µm';
            end else begin
              FCPUName:='VIA C3 C5C';
              FCodename:='Ezra';
              FTech:='0.13µm';
            end;
         8: begin
           FCPUName:='VIA C3 C5N';
           FCodename:='Ezra-T';
           FTech:='0.13 µm';
         end;
         9: if FStepping<8 then begin
              FCPUName:='VIA C3 C5XL';
              FCodename:='Nehemiah';
              FTech:='0.13 µm';
            end else begin
              FCPUName:='VIA C3 C5P';
              FCodename:='Nehemiah';
              FTech:='0.13 µm';
            end;
       end;
  end;
end;

{procedure TMiTeC_CPU.IntelLookupCode;
const
  CoreArray: array[0..4] of record
                              Core: string;
                              Model: array[0..4] of string;
                            end = (
    (Core:'Conroe';Model:('E6','X6','','','')),
    (Core:'Allendale';Model:('E4','E2','','','')),
    (Core:'Kentsfield';Model:('Q6','QX6','','','')),
    (Core:'Merom';Model:('T7','T5','L7','X7','U7')),
    (Core:'Yonah';Model:('T2','T1','L2','U2',''))
  );
var
  i,j: Integer;
begin
  if FCodeName<>'' then
    Exit;
  for i:=0 to High(CoreArray) do
    for j:=0 to High(CoreArray[i].Model) do
      if Pos(CoreArray[i].Model[j],FCPUName)>0 then begin
        FCodeName:=CoreArray[i].Core;
        Break;
      end;
end;}

procedure TMiTeC_CPU.IntelLookupName;
begin
  case FFamily of
    4: case FModel of
         0: begin
           FCPUName:='i80486DX';
           FCodename:='P4';
           case FStepping of
             0: FRevision:='A0-A1';
             1: FRevision:='B2-B6';
             2: FRevision:='C0';
             3: FRevision:='C1';
             4: FRevision:='D0';
           end;
         end;
         1: begin
           FCPUName:='i80486DX';
           if FStepping in [4,5] then
             FCPUName:=FCPUName+'-SL';
           FCodename:='P4';
           case FStepping of
             0: FRevision:='cA2,cA3';
             1: FRevision:='cB0,cB1';
             3: FRevision:='cC0';
             4: FRevision:='aA0,aA1';
             5: FRevision:='aB0';
           end;
         end;
         2: begin
           FCPUName:='i80486SX';
           if FStepping=3 then
             FCPUName:=FCPUName+'-WB';
           if FStepping in [10..11] then
             FCPUName:=FCPUName+'-SL';
           FCodename:='P4S';
           case FStepping of
             0: FRevision:='A0';
             2: FRevision:='B0';
             3: FRevision:='bBx';
             4: FRevision:='gAx';
             7: FRevision:='cA0';
             8: FRevision:='cB0';
             10: FRevision:='aA0,aA1';
             11: FRevision:='aB0,aC0';
           end;
         end;
         3: begin
           FCPUName:='i80486DX/2';
           if FStepping in [4,5] then
             FCPUName:=FCPUName+'-SL';
           FCodename:='P24S';
           if FStepping=6 then begin
             FCPUName:=FCPUName+'-WB';
             FCodeName:='P24D';
           end;
           case FStepping of
             2: FRevision:='A0-A2';
             3: FRevision:='B1';
             4: FRevision:='aA0,aA1';
             5: FRevision:='aB0,aC0';
             6: FRevision:='A';
           end;
         end;
         4: begin
           FCPUName:='i80486SL';
           FCodename:='P23';
           if FStepping=0 then
             FRevision:='A';
         end;
         5: begin
           FCPUName:='i80486SX/2';
           FCodename:='P23';
           if FStepping=11 then
             FRevision:='aC0';
         end;
         7: begin
           FCPUName:='i80486DX/2-WB';
           FCodename:='P24D';
           FRevision:='A';
         end;
         8,9: begin
           FCPUName:='i80486DX/4';
           if FCPUType=1 then
             FCPUName:=FCPUName+' OverDrive';
           FCodename:='P24C';
           FRevision:='A';
         end;
    end;
    5: case FModel of
         0: begin
           FCPUName:='Pentium';
           FCodename:='A80501,P5';
           FRevision:='Ax';
           FTech:='0.80 µm';
         end;
         1: begin
           FCPUName:='Pentium';
           FCodename:='A80501,P5';
           FTech:='0.80 µm';
           if FCPUType=1 then begin
             FCPUName:='Pentium OverDrive for P5';
             FCodename:='PODP5V,P5T';
             if FStepping=10 then
               FRevision:='tA0';
           end else
             case FStepping of
               3: FRevision:='B1';
               4: FRevision:='B2';
               5: FRevision:='C1';
               7: FRevision:='D1';
             end;
         end;
         2: begin
           {FCPUName:='Mobile Pentium';
           FCodeName:='P54LM';
           case FStepping of
             5: FRevision:='A1,mA1';
             11: FRevision:='mcB1';
             12: FRevision:='mcC0';
           end;}
           case FCPUType of
             0: begin
               FCPUName:='Pentium';
               FCodename:='A80502,P54C,P54CS';
               FTech:='0.50 µm';
               case FStepping of
                 1: FRevision:='B1';
                 2: FRevision:='B3';
                 4: FRevision:='B5';
                 5: FRevision:='C1,C2,mA1';
                 6: FRevision:='E0';
                 11: FRevision:='cB1,mcB1';
                 12: FRevision:='aC0,cC0,mcC0,acC0';
               end;
             end;
             1: begin
               FCPUName:='Pentium OverDrive for P54C';
               FCodename:='P54CT';
               FTech:='0.35 µm';
             end;
             2: begin
               FCPUName:='Pentium OverDrive for P54C';
               FCodename:='P54M';
               FTech:='0.35 µm';
             end;
           end;
         end;
         3: begin
           FCPUName:='Pentium OverDrive for 486';
           if FStepping=1 then
             FCodename:='P24T'
           else
             FCodename:='P24B'
         end;
         4: begin
           FCPUName:='Pentium MMX';
           FCodename:='A80503,P55C';
           FTech:='0.28 µm';
           if FCPUType=1 then begin
             FCPUName:='Pentium MMX OverDrive for P54C';
             FCodename:='P55CTP';
             if FStepping=4 then
               FRevision:='oxA3';
           end else
             case FStepping of
               1: FRevision:='A1';
               2: FRevision:='A3';
               3: FRevision:='xB1,mxB1';
               4: FRevision:='xA3,mxA3';
             end;
         end;
         7: begin
           FCPUName:='Pentium';
           FCodename:='A80502,P54C,P54CS';
           FTech:='0.35 µm';
         end;
         8: begin
           FCPUName:='Pentium MMX';
           FCodename:='A80503,P55C';
           FTech:='0.25 µm';
           case FStepping of
             1: FRevision:='myA0';
             2: FRevision:='sB1,myB1';
           end;
         end;
    end;
    6: case FModel of
         0: begin
           FCPUName:='Pentium Pro';
           FCodename:='A80521,P6';
           FTech:='0.50 µm';
         end;
         1: begin
           FCPUName:='Pentium Pro';
           FCodename:='A80521,P6';
           FTech:='0.35 µm';
           case FStepping of
             1: FRevision:='B0';
             2: FRevision:='C0';
             6: FRevision:='sA0';
             7: FRevision:='sA1';
             9: FRevision:='sB1';
           end;
         end;
         3: if FCPUType=0 then begin
           FCPUName:='Pentium II';
           FCodename:='A80522,P6L Klamath';
           FTech:='0.28 µm';
           case FStepping of
             3: FRevision:='C0';
             4: FRevision:='C1';
           end;
         end else begin
           FCPUName:='Pentium II OverDrive';
           FCodename:='POPD66X333,P6T';
           FTech:='0.28 µm';
           if FStepping=2 then
             FRevision:='TdB0';
         end;
         5: begin
           FTech:='0.25 µm';
           if FCPUCache.Level2.Size>=1024 then begin
             FCPUName:='Pentium II Xeon';
             FCodename:='A80523, P6L Deschutes';
           end else
             if FCPUCache.Level2.Size=512 then begin
               FCPUName:='Pentium II';
               FCodename:='A80523, P6L Deschutes';
             end else
               if FCPUCache.Level2.Size=256 then begin
                 FCPUName:='Pentium II PE';
                 FCodename:='Tonga';
               end else begin
                 FCPUName:='Celeron';
                 FCodename:='P6C Covington';
               end;
           case FStepping of
             0: FRevision:='dA0,mdA0,mmdA0';
             1: FRevision:='dA1';
             2: FRevision:='B0,dB0,mdB0,mmdB0';
             3: FRevision:='B1,dB1';
           end;
         end;
         6: begin
           FCodename:='Dixon';
           FTech:='0.25 µm';
           if FCPUCache.Level2.Size=128 then begin
             if FCPUFeatures.Standard1.FeaturesByFlag[SFS_PSN].Available then
               FCPUName:='Mobile Celeron'
             else begin
               FCPUName:='Celeron';
               FCodename:='P6C Mendocino, Celeron A';
             end;
           end else
             FCPUName:='Mobile Pentium II';
           case FStepping of
             0: FRevision:='mA0';
             2: FRevision:='dB1';
             5: FRevision:='mB0';
             10: FRevision:='mdbA0,mdxA0,mqbA1,mqpA1';
           end;
         end;
         7: begin
           FTech:='0.25 µm';
           if FCPUCache.Level2.Size<1024 then begin
             FCPUName:='Pentium III';
             FCodename:='A80525,P6K Katmai';
           end else begin
             FCPUName:='Pentium III Xeon';
             FCodename:='Tanner';
           end;
           case FStepping of
             2: FRevision:='B0,kB0';
             3: FRevision:='C0,kC0';
           end;
         end;
         8: begin
           FTech:='0.18 µm';
           if FCPUCache.Level2.Size<=128 then begin
             FCPUName:='Celeron';
             FCodename:='Coppermine-128, Celeron II';
           end else
             case FBrand of
               1: begin
                 FCPUName:='Celeron';
                 FCodename:='A80526, Coppermine';
               end;
               3: begin
                 FCPUName:='Pentium III Xeon';
                 FCodename:='Cascades';
               end
               else begin  //2,4
                 FCPUName:='Pentium III';
                 FCodename:='A80526, Coppermine';
               end;
             end;
           case FStepping of
             1: FRevision:='A2,cA2,cA2c';
             3: FRevision:='B0,cB0,cB0c';
             6: FRevision:='C0,cC0';
             10: FRevision:='D0,cD0';
           end;
         end;
         9: begin
           FTech:='0.13 µm';
           FCPUName:='Pentium III';
           case FBrand of
             18: begin
               FCPUName:='Celeron M';
               FCodename:='Banias';
               FTech:='0.13 µm';
             end;
             22: begin
               FCPUName:='Pentium M';
               FCodename:='Banias';
               FTech:='0.13 µm';
             end;
           end;
         end;
         10: begin
           FCPUName:='Pentium III Xeon A';
           FCodename:='A80530,Tualatin';
           FTech:='0.18 µm';
           case FStepping of
             0: FRevision:='A0';
             1: FRevision:='A1';
             4: FRevision:='B0';
           end;
         end;
         11: begin
           FTech:='0.13 µm';
           FCPUName:='Pentium III B';
           FCodename:='A80530,Tualatin';
           case FBrand of
             1: FCPUName:='Celeron';
             3: if FCPUCache.Level2.Size>256 then
                  FCPUName:='Pentium III Xeon'
                else
                  FCPUName:='Celeron';
             6: begin
               FCPUName:='Mobile Pentium III M';
               FCodename:='Geyservile';
             end;
             7: begin
               FCPUName:='Mobile Celeron';
               FCodename:='Geyservile';
             end;
           end;
           case FStepping of
             1: FRevision:='tA1,A1';
             4: FRevision:='tB1';
           end;
         end;
         12: begin
           FCPUName:='';
           FTech:='0.09 µm';
           FCodename:='Dothan';
         end;
         13: begin
           FCPUName:='';
           FTech:='0.09 µm';
           FCodename:='Dothan';
           case FStepping of
             8: if FCPUCache.Level2.Size=4096 then
                 FCodename:='Yonah';
             else FCodename:='Dothan';
           end;
         end;
         14: begin
           FCPUName:='';
           FTech:='65 nm';
           case FStepping of
             8,12: FCodename:='Yonah';
             else FCodename:='Dothan';
           end;
         end;
         15: begin
           FCPUName:='';
           FTech:='65 nm';
           case FStepping of
             1: begin
               FCodename:='Conroe';
               if ibXeon in FIntelBrand then
                 FCodename:='Woodcrest';
             end;
             else FCodename:='Merom';
           end;
         end;
    end;
    7: begin
      FCPUName:='Itanium';
      FCodename:='Merced';
      FTech:='0.18 µm';
    end;
    15: case FFamilyEx of
          0: begin
            FCPUName:='Pentium 4';
            if ibCeleron in FIntelBrand then
              FCPUName:='Celeron 4';
            if ibMobile in FIntelBrand then
              FCPUName:=FCPUName+' Mobile';
            if ibM in FIntelBrand then
              FCPUName:=FCPUName+' M';
            if (ibXeon in FIntelBrand) or (CPUPhysicalCount>1) then
              FCPUName:=FCPUName+' Xeon';
            if ibMP in FIntelBrand then
              FCPUName:=FCPUName+' MP';
            if (FCPUCache.Level2.Size<=128) then
              FCPUName:='Celeron';
            if (FCPUCache.Level2.Size=256) and (FIntelBrand=[]) then
              FCPUName:='Celeron D';

            case FModel of
              0,1: begin
                FCodename:='P68, Willamette';
                FTech:='0.18 µm';
                if ibXeon in FIntelBrand then begin
                  FCodename:='Foster';
                  if ibMP in FIntelBrand then
                    FCodename:=FCodename+' MP';
                end;
                case FStepping of
                  2: FRevision:='D0';
                  3: FRevision:='E0';
                  7: FRevision:='B2';
                  10: FRevision:='C1';
                end;
              end;
              2: begin
                FCodename:='Northwood';
                FTech:='0.13 µm';
                if (ibXeon in FIntelBrand) then
                  FCodename:='Prestonia';
                if (ibMP in FIntelBrand) then
                  FCodename:='Gallatin';
                case FStepping of
                  4: FRevision:='B0';
                  5: FRevision:='M0';
                  7: FRevision:='C1';
                  9: FRevision:='D1';
                end;
              end;
              3: begin
                FCodename:='Prescott';
                FTech:='90 nm';
                if (ibXeon in FIntelBrand) then
                  FCodename:='Nocona';
                if (ibMP in FIntelBrand) then
                  FCodename:='Potomac';
              end;
              4: begin
                FTech:='90 nm';
                FCodeName:='Prescott';
                if (ibCeleron in FIntelBrand) then  begin
                  if (FCPUCache.Level2.Size=256) then
                    FCPUName:='Celeron D'
                  else
                    FCPUName:='Celeron';
                end;
                if (ibXeon in FIntelBrand) then
                  FCodename:='Nocona';
                if (ibMP in FIntelBrand) then
                  FCodename:='Potomac';
                if (FStepping=1) and (ibXeon in FIntelBrand) and (ibMP in FIntelBrand) then begin
                  FCPUName:='Pentium 4';
                  FCodeName:='Cranford';
                end;
                if FStepping in [3,4,7] then begin
                  FCPUName:='Pentium D';
                  FCodeName:='Smithfield';
                end;
                if (FStepping=3) and (ibXeon in FIntelBrand) and (ibMP in FIntelBrand) then begin
                  FCPUName:='Pentium 4 DP';
                  FCodeName:='Irwindale';
                end;
              end;
              6: begin
                FTech:='90 nm';
                FCodeName:='Presler';
                FCPUName:='Pentium D';
              end;
            end;
          end;
          1: begin
            FCPUName:='Itanium 2';
            FTech:='0.13 µm';
            case FModel of
              0: begin
                FCodename:='McKinley';
                FTech:='0.18 µm';
              end;
              1: FCodename:='Madison/Deerfield';
              2: FCodename:='Madison 9M';
            end;
          end;
        end;
    16: begin
            FCPUName:='Pentium 4';
            if ibCeleron in FIntelBrand then
              FCPUName:='Celeron 4';
            if ibMobile in FIntelBrand then
              FCPUName:=FCPUName+' Mobile';
            if ibM in FIntelBrand then
              FCPUName:=FCPUName+' M';
            if (ibXeon in FIntelBrand) or (CPUPhysicalCount>1) then
              FCPUName:=FCPUName+' Xeon';
            if ibMP in FIntelBrand then
              FCPUName:=FCPUName+' MP';
            if (FCPUCache.Level2.Size<=128) then
              FCPUName:='Celeron';
            if (FCPUCache.Level2.Size=256) and (FIntelBrand=[]) then
              FCPUName:='Celeron D';
            if FCPUFeatures.Standard1.GetFeatureByFlag(SFS_IA64).Available or (Architecture=PROCESSOR_ARCHITECTURE_AMD64) then
              FCPUName:='64bit '+FCPUName;
    end;
  end;
end;

function TMiTeC_CPU.LoadFromStorage;
var
  stg: IStorage;
  SS,Sub: TStructuredStorage;

function ReadFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
  ib: TIntelBrand;
  i,n: Integer;
begin
  Result:=False;
  try
    strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_READ_INSTORAGE,False)
  except
    strm:=nil;
  end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        Self.FCount:=ReadIntProperty(sl,'CPUCount');
        Self.FPC:=ReadIntProperty(sl,'CPUPhysicalCount');
        Self.FCPUType:=ReadIntProperty(sl,'CPUType');
        Self.FCPUIDSupported:=ReadIntProperty(sl,'CPUIDSupported')=1;
        Self.FArch:=ReadIntProperty(sl,'Architecture');
        Self.FVendor:=TCPUVendor(ReadIntProperty(sl,'Vendor'));
        Self.FGenericName:=ReadStrProperty(sl,'GenericName');
        Self.FCPUName:=ReadStrProperty(sl,'CPUName');
        Self.FMarketingName:=ReadStrProperty(sl,'MarketingName');
        Self.FCodeName:=ReadStrProperty(sl,'CodeName');
        Self.FFreq:=ReadIntProperty(sl,'Frequency');
        Self.FAPICID:=ReadIntProperty(sl,'APICID');
        Self.FLID:=ReadIntProperty(sl,'LogicalID');
        Self.FPID:=ReadIntProperty(sl,'PhysicalID');
        Self.FLPP:=ReadIntProperty(sl,'LogicalPerPackage');
        Self.FCPP:=ReadIntProperty(sl,'CorePerPackage');
        Self.FLPC:=ReadIntProperty(sl,'LogicalPerCore');
        Self.FCC:=ReadIntProperty(sl,'CoreCount');
        Self.FTC:=ReadIntProperty(sl,'ThreadCount');
        Self.FFamily:=ReadIntProperty(sl,'Family');
        Self.FModel:=ReadIntProperty(sl,'Model');
        Self.FStepping:=ReadIntProperty(sl,'Stepping');
        Self.FFamilyEx:=ReadIntProperty(sl,'FamilyEx');
        Self.FModelEx:=ReadIntProperty(sl,'ModelEx');
        Self.FSteppingEx:=ReadIntProperty(sl,'SteppingEx');
        Self.FBrand:=ReadIntProperty(sl,'Brand');
        n:=ReadIntProperty(sl,'IntelBrand');
        Self.FIntelBrand:=[];
        for ib:=Low(TIntelBrand) to High(TIntelBrand) do
          if n and Round(Power(2,Integer(ib)))<>0 then
            Self.FIntelBrand:=Self.FIntelBrand+[ib];

        Self.FRevision:=ReadStrProperty(sl,'Revision');
        Self.FTech:=ReadStrProperty(sl,'Technology');
        Self.FSN:=ReadStrProperty(sl,'SerialNumber');
        Self.FFDIVBug:=ReadIntProperty(sl,'FDIVBug')=1;

        Self.FCPUCache.FLevel1.FData.FSize:=ReadIntProperty(sl,'Cache.Level1.Data.Size');
        Self.FCPUCache.FLevel1.FData.FLineSize:=ReadIntProperty(sl,'Cache.Level1.Data.LineSize');
        Self.FCPUCache.FLevel1.FData.FAssociativity:=TCacheAssociativity(ReadIntProperty(sl,'Cache.Level1.Data.Associativity'));
        Self.FCPUCache.FLevel1.FData.FLevel:=ReadIntProperty(sl,'Cache.Level1.Data.Level');
        Self.FCPUCache.FLevel1.FData.FShared:=ReadIntProperty(sl,'Cache.Level1.Data.SharedWays');
        Self.FCPUCache.FLevel1.FData.FWays:=ReadIntProperty(sl,'Cache.Level1.Data.Ways');
        Self.FCPUCache.FLevel1.FData.FParts:=ReadIntProperty(sl,'Cache.Level1.Data.Partitions');
        Self.FCPUCache.FLevel1.FData.FTyp:=ReadIntProperty(sl,'Cache.Level1.Data.Typ');
        if Self.FCPUCache.FLevel1.FData.FShared=0 then
          Self.FCPUCache.FLevel1.FData.FShared:=1;

        Self.FCPUCache.FLevel1.FCode.FSize:=ReadIntProperty(sl,'Cache.Level1.Code.Size');
        Self.FCPUCache.FLevel1.FCode.FLineSize:=ReadIntProperty(sl,'Cache.Level1.Code.LineSize');
        Self.FCPUCache.FLevel1.FCode.FAssociativity:=TCacheAssociativity(ReadIntProperty(sl,'Cache.Level1.Code.Associativity'));
        Self.FCPUCache.FLevel1.FCode.FShared:=ReadIntProperty(sl,'Cache.Level1.Code.SharedWays');
        Self.FCPUCache.FLevel1.FCode.FWays:=ReadIntProperty(sl,'Cache.Level1.Code.Ways');
        Self.FCPUCache.FLevel1.FCode.FParts:=ReadIntProperty(sl,'Cache.Level1.Code.Partitions');
        Self.FCPUCache.FLevel1.FCode.FTyp:=ReadIntProperty(sl,'Cache.Level1.Code.Typ');
        if Self.FCPUCache.FLevel1.FCode.FShared=0 then
          Self.FCPUCache.FLevel1.FCode.FShared:=1;

        Self.FCPUCache.FLevel1.FUnified.FSize:=ReadIntProperty(sl,'Cache.Level1.Unified.Size');
        Self.FCPUCache.FLevel1.FUnified.FLineSize:=ReadIntProperty(sl,'Cache.Level1.Unified.LineSize');
        Self.FCPUCache.FLevel1.FUnified.FAssociativity:=TCacheAssociativity(ReadIntProperty(sl,'Cache.Level1.Unified.Associativity'));
        Self.FCPUCache.FLevel1.FUnified.FShared:=ReadIntProperty(sl,'Cache.Level1.Unified.SharedWays');
        Self.FCPUCache.FLevel1.FUnified.FWays:=ReadIntProperty(sl,'Cache.Level1.Unified.Ways');
        Self.FCPUCache.FLevel1.FUnified.FParts:=ReadIntProperty(sl,'Cache.Level1.Unified.Partitions');
        Self.FCPUCache.FLevel1.FUnified.FTyp:=ReadIntProperty(sl,'Cache.Level1.Unified.Typ');
        if Self.FCPUCache.FLevel1.FUnified.FShared=0 then
          Self.FCPUCache.FLevel1.FUnified.FShared:=1;

        Self.FCPUCache.FLevel2.FSize:=ReadIntProperty(sl,'Cache.Level2.Size');
        Self.FCPUCache.FLevel2.FLineSize:=ReadIntProperty(sl,'Cache.Level2.LineSize');
        Self.FCPUCache.FLevel2.FAssociativity:=TCacheAssociativity(ReadIntProperty(sl,'Cache.Level2.Associativity'));
        Self.FCPUCache.FLevel2.FShared:=ReadIntProperty(sl,'Cache.Level2.SharedWays');
        Self.FCPUCache.FLevel2.FWays:=ReadIntProperty(sl,'Cache.Level2.Ways');
        Self.FCPUCache.FLevel2.FParts:=ReadIntProperty(sl,'Cache.Level2.Partitions');
        Self.FCPUCache.FLevel2.FTyp:=ReadIntProperty(sl,'Cache.Level2.Typ');
        if Self.FCPUCache.FLevel2.FShared=0 then
          Self.FCPUCache.FLevel2.FShared:=1;

        Self.FCPUCache.FLevel3.FSize:=ReadIntProperty(sl,'Cache.Level3.Size');
        Self.FCPUCache.FLevel3.FLineSize:=ReadIntProperty(sl,'Cache.Level3.LineSize');
        Self.FCPUCache.FLevel3.FAssociativity:=TCacheAssociativity(ReadIntProperty(sl,'Cache.Level3.Associativity'));
        Self.FCPUCache.FLevel3.FShared:=ReadIntProperty(sl,'Cache.Level3.SharedWays');
        Self.FCPUCache.FLevel3.FWays:=ReadIntProperty(sl,'Cache.Level3.Ways');
        Self.FCPUCache.FLevel3.FParts:=ReadIntProperty(sl,'Cache.Level3.Partitions');
        Self.FCPUCache.FLevel3.FTyp:=ReadIntProperty(sl,'Cache.Level3.Typ');
        if Self.FCPUCache.FLevel3.FShared=0 then
          Self.FCPUCache.FLevel3.FShared:=1;

        Self.FCPUCache.FTrace.FSize:=ReadIntProperty(sl,'Cache.Trace.Size');
        Self.FCPUCache.FTrace.FLineSize:=ReadIntProperty(sl,'Cache.Trace.LineSize');
        Self.FCPUCache.FTrace.FAssociativity:=TCacheAssociativity(ReadIntProperty(sl,'Cache.Trace.Associativity'));

        with Self.Features.Standard1 do begin
          GetAvailableFeatures(fsStandard1,FAF);
            for i:=0 to High(FAF) do
              FAF[i].Available:=ReadIntProperty(sl,Format('Features.Standard1.%s',[FAF[i].Mnemonic]))=1;
        end;

        with Self.Features.Standard2 do begin
          GetAvailableFeatures(fsStandard2,FAF);
          for i:=0 to High(FAF) do
            FAF[i].Available:=ReadIntProperty(sl,Format('Features.Standard2.%s',[FAF[i].Mnemonic]))=1;
        end;

        with Self.Features.Extended1 do begin
          GetAvailableFeatures(fsExtended1,FAF);
          for i:=0 to High(FAF) do
            FAF[i].Available:=ReadIntProperty(sl,Format('Features.Extended1.%s',[FAF[i].Mnemonic]))=1;
        end;

        with Self.Features.Extended2 do begin
          GetAvailableFeatures(fsExtended2,FAF);
          for i:=0 to High(FAF) do
            FAF[i].Available:=ReadIntProperty(sl,Format('Features.Extended2.%s',[FAF[i].Mnemonic]))=1;
        end;

        with Self.Features.PowerManagement do begin
          GetAvailableFeatures(fsPowerManagement,FAF);
          for i:=0 to High(FAF) do
            FAF[i].Available:=ReadIntProperty(sl,Format('Features.PowerManagement.%s',[FAF[i].Mnemonic]))=1;
        end;
        Result:=True;
        SetDataAvail(True);
      finally
        sl.Free;
      end;
    finally
      strm.Free;
    end;
end;

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
      Exit;
    end;
    try
      Result:=ReadFromStream(CPUIndex);
      if Self.FCC=0 then
        FCC:=CPUPhysicalCount*CorePerPackage;
      if Self.FTC=0 then
        FTC:=CPUPhysicalCount*LogicalPerPackage;
    finally
      if Assigned(Sub) then
        Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;


function TMiTeC_CPU.LookupAssociativity(Value: Byte): TCacheAssociativity;
var
  i: TCacheAssociativity;
begin
  Result:=caNone;
  for i:=Low(TCacheAssociativity) to High(TCacheAssociativity) do
    if Value=cAssociativityInfo[i] then begin
      Result:=i;
      Break;
    end;
end;

procedure TMiTeC_CPU.NexGenLookupName;
begin
  case FFamily of
    5: case FModel of
         0: begin
           FCPUName:='Nx586';
           FTech:='0.50-0.44 µm';
         end;
         6: begin
           FCPUName:='Nx686';
           FCodename:='HA';
           FTech:='0,50 µm';
         end;
       end;
  end;
end;

procedure TMiTeC_CPU.RiseLookupName;
begin
  case FFamily of
    5: case FModel of
         0: begin
           FCPUName:='mP6';
           FCodename:='iDragon';
         end;
         2: begin
           FCPUName:='mP6';
           FCodename:='iDragon';
           FTech:='0.18 µm';
         end;
         8: begin
           FCPUName:='mP6';
           FCodename:='iDragon II';
           FTech:='0.25 µm';
         end;
         9: begin
           FCPUName:='mP6';
           FCodename:='iDragon II';
           FTech:='0.18 µm';
         end;
       end;
  end;
end;

procedure TMiTeC_CPU.SiSLookupName;
begin
  case FFamily of
    5: case FModel of
      0: FCPUName:='55x';
    end;
  end;
end;

procedure TMiTeC_CPU.TransmetaLookupName;
begin
  case FFamily of
    5: begin
      FCPUName:='Crusoe';
      if FCPUCache.Level2.Size=0 then
        FCodename:='TM3200'
      else
        if FCPUCache.Level2.Size=256 then
          FCodename:='TM5400/TM5500'
        else
          FCodename:='TM5600/TM5800';
    end;
    6: begin
      FCPUName:='Efficeon';
      if FCPUCache.Level2.Size=256 then
        FCodename:='Astro,TM8300/TM8500'
      else
        FCodename:='Astro,TM8600/TM8800';
    end;
  end;
end;

procedure TMiTeC_CPU.UMCLookupName;
begin
  case FFamily of
    4: case FModel of
         1: FCPUName:='U5SD';
         2: FCPUName:='U5S/X';
         3: FCPUName:='U486DX2';
         5: FCPUName:='U486SX2';
        end;
  end;
end;

function TMiTeC_CPU.ValidDescriptor(Value: Cardinal): Boolean;
begin
  Result:=(Value and (1 shl 31))=0;
end;


procedure TMiTeC_CPU.SaveToStorage;
var
  stg: IStorage;
  SS, Sub: TStructuredStorage;

procedure WriteToStream(AIndex: integer);
var
  strm: TStorageStream;
  sl: TStringList;
  ib: TIntelBrand;
  i,n: Integer;
begin
    sl:=TStringList.Create;
    try
      WriteIntProperty(sl,'CPUCount',Self.CPUCount);
      WriteIntProperty(sl,'CPUPhysicalCount',Self.CPUPhysicalCount);
      WriteIntProperty(sl,'CPUType',Self.CpuType);
      WriteIntProperty(sl,'CPUIDSupported',Integer(Self.CPUIDSupported));
      WriteIntProperty(sl,'Architecture',Integer(Self.Architecture));
      WriteIntProperty(sl,'Vendor',Integer(Self.Vendor));
      WriteStrProperty(sl,'CPUName',Self.CPUName);
      WriteStrProperty(sl,'GenericName',Self.GenericName);
      WriteStrProperty(sl,'MarketingName',Self.MarketingName);
      WriteStrProperty(sl,'CodeName',Self.CodeName);
      WriteIntProperty(sl,'Frequency',Self.Frequency);
      WriteIntProperty(sl,'APICID',Self.APICID);
      WriteIntProperty(sl,'LogicalID',Self.LogicalID);
      WriteIntProperty(sl,'PhysicalID',Self.PhysicalID);
      WriteIntProperty(sl,'LogicalPerPackage',Self.LogicalPerPackage);
      WriteIntProperty(sl,'CorePerPackage',Self.CorePerPackage);
      WriteIntProperty(sl,'LogicalPerCore',Self.LogicalPerCore);
      WriteIntProperty(sl,'CoreCount',Self.CoreCount);
      WriteIntProperty(sl,'ThreadCount',Self.ThreadCount);
      WriteIntProperty(sl,'Family',Self.Family);
      WriteIntProperty(sl,'Model',Self.Model);
      WriteIntProperty(sl,'Stepping',Self.Stepping);
      WriteIntProperty(sl,'FamilyEx',Self.FamilyEx);
      WriteIntProperty(sl,'ModelEx',Self.ModelEx);
      WriteIntProperty(sl,'SteppingEx',Self.SteppingEx);
      WriteIntProperty(sl,'Brand',Self.Brand);
      n:=0;
      for ib:=Low(TIntelBrand) to High(TIntelBrand) do
        if ib in Self.IntelBrand then
          n:=n+Round(Power(2,Integer(ib)));
      WriteIntProperty(sl,'IntelBrand',n);
      WriteStrProperty(sl,'Revision',Self.Revision);
      WriteStrProperty(sl,'Technology',Self.Technology);
      WriteStrProperty(sl,'SerialNumber',Self.SerialNumber);
      WriteIntProperty(sl,'FDIVBug',Integer(Self.FDIVBug));

      WriteIntProperty(sl,'Cache.Level1.Data.Typ',Self.Cache.Level1.Data.Typ);
      WriteIntProperty(sl,'Cache.Level1.Data.Level',Self.Cache.Level1.Data.Level);
      WriteIntProperty(sl,'Cache.Level1.Data.Ways',Self.Cache.Level1.Data.Ways);
      WriteIntProperty(sl,'Cache.Level1.Data.Partitions',Self.Cache.Level1.Data.Partitions);
      WriteIntProperty(sl,'Cache.Level1.Data.SharedWays',Self.Cache.Level1.Data.SharedWays);
      WriteIntProperty(sl,'Cache.Level1.Data.Size',Self.Cache.Level1.Data.Size);
      WriteIntProperty(sl,'Cache.Level1.Data.LineSize',Self.Cache.Level1.Data.LineSize);
      WriteIntProperty(sl,'Cache.Level1.Data.Associativity',integer(Self.Cache.Level1.Data.Associativity));

      WriteIntProperty(sl,'Cache.Level1.Code.Typ',Self.Cache.Level1.Code.Typ);
      WriteIntProperty(sl,'Cache.Level1.Code.Level',Self.Cache.Level1.Code.Level);
      WriteIntProperty(sl,'Cache.Level1.Code.Ways',Self.Cache.Level1.Code.Ways);
      WriteIntProperty(sl,'Cache.Level1.Code.Partitions',Self.Cache.Level1.Code.Partitions);
      WriteIntProperty(sl,'Cache.Level1.Code.SharedWays',Self.Cache.Level1.Code.SharedWays);
      WriteIntProperty(sl,'Cache.Level1.Code.Size',Self.Cache.Level1.Code.Size);
      WriteIntProperty(sl,'Cache.Level1.Code.LineSize',Self.Cache.Level1.Code.LineSize);
      WriteIntProperty(sl,'Cache.Level1.Code.Associativity',integer(Self.Cache.Level1.Code.Associativity));

      WriteIntProperty(sl,'Cache.Level1.Unified.Typ',Self.Cache.Level1.Unified.Typ);
      WriteIntProperty(sl,'Cache.Level1.Unified.Level',Self.Cache.Level1.Unified.Level);
      WriteIntProperty(sl,'Cache.Level1.Unified.Ways',Self.Cache.Level1.Unified.Ways);
      WriteIntProperty(sl,'Cache.Level1.Unified.Partitions',Self.Cache.Level1.Unified.Partitions);
      WriteIntProperty(sl,'Cache.Level1.Unified.SharedWays',Self.Cache.Level1.Unified.SharedWays);
      WriteIntProperty(sl,'Cache.Level1.Unified.Size',Self.Cache.Level1.Unified.Size);
      WriteIntProperty(sl,'Cache.Level1.Unified.LineSize',Self.Cache.Level1.Unified.LineSize);
      WriteIntProperty(sl,'Cache.Level1.Unified.Associativity',integer(Self.Cache.Level1.Unified.Associativity));

      WriteIntProperty(sl,'Cache.Level2.Typ',Self.Cache.Level2.Typ);
      WriteIntProperty(sl,'Cache.Level2.Level',Self.Cache.Level2.Level);
      WriteIntProperty(sl,'Cache.Level2.Ways',Self.Cache.Level2.Ways);
      WriteIntProperty(sl,'Cache.Level2.Partitions',Self.Cache.Level2.Partitions);
      WriteIntProperty(sl,'Cache.Level2.Size',Self.Cache.Level2.Size);
      WriteIntProperty(sl,'Cache.Level2.SharedWays',Self.Cache.Level2.SharedWays);
      WriteIntProperty(sl,'Cache.Level2.LineSize',Self.Cache.Level2.LineSize);
      WriteIntProperty(sl,'Cache.Level2.Associativity',integer(Self.Cache.Level2.Associativity));

      WriteIntProperty(sl,'Cache.Level3.Typ',Self.Cache.Level3.Typ);
      WriteIntProperty(sl,'Cache.Level3.Level',Self.Cache.Level3.Level);
      WriteIntProperty(sl,'Cache.Level3.Ways',Self.Cache.Level3.Ways);
      WriteIntProperty(sl,'Cache.Level3.Partitions',Self.Cache.Level3.Partitions);
      WriteIntProperty(sl,'Cache.Level3.Size',Self.Cache.Level3.Size);
      WriteIntProperty(sl,'Cache.Level3.SharedWays',Self.Cache.Level3.SharedWays);
      WriteIntProperty(sl,'Cache.Level3.LineSize',Self.Cache.Level3.LineSize);
      WriteIntProperty(sl,'Cache.Level3.Associativity',integer(Self.Cache.Level3.Associativity));

      WriteIntProperty(sl,'Cache.Trace.Size',Self.Cache.Trace.Size);
      WriteIntProperty(sl,'Cache.Trace.LineSize',Self.Cache.Trace.LineSize);
      WriteIntProperty(sl,'Cache.Trace.Associativity',integer(Self.Cache.Trace.Associativity));

      with Self.Features.Standard1 do
        for i:=0 to Count-1 do
         WriteIntProperty(sl,Format('Features.Standard1.%s',[Features[i].Mnemonic]),Integer(Features[i].Available));

      with Self.Features.Standard2 do
        for i:=0 to Count-1 do
          WriteIntProperty(sl,Format('Features.Standard2.%s',[Features[i].Mnemonic]),Integer(Features[i].Available));

      with Self.Features.Extended1 do
        for i:=0 to Count-1 do
          WriteIntProperty(sl,Format('Features.Extended1.%s',[Features[i].Mnemonic]),Integer(Features[i].Available));

     with Self.Features.Extended2 do
        for i:=0 to Count-1 do
          WriteIntProperty(sl,Format('Features.Extended2.%s',[Features[i].Mnemonic]),Integer(Features[i].Available));

      with Self.Features.PowerManagement do
        for i:=0 to Count-1 do
          WriteIntProperty(sl,Format('Features.PowerManagement.%s',[Features[i].Mnemonic]),Integer(Features[i].Available));
      strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_OPEN,True);
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
  i,c,n: Integer;
  s: string;
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
      s:='';
      n:=0;
      c:=CPUCount;
      for i:=0 to c-1 do
        if LiveData then begin
          CPUIndex:=i;
          RefreshData;
          if not ((Pos(IntToStr(PhysicalID),s)=0) or ((i=c-1) and (n<CPUPhysicalCount))) then
            Continue;
          s:=s+IntToStr(PhysicalID)+',';
          WriteToStream(n);
          Inc(n);
        end;
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
    CPUIndex:=0;
  end;
end;

procedure TMiTeC_CPU.SetIndex(const Value: Byte);
begin
  FIndex:=Value;
end;

function TMiTeC_CPU.GetIntelBrand: TIntelBrands;
begin
  Result:=[];
  case FBrand of
    $1: Result:=Result+[ibCeleron];
    $2,$4: Result:=Result+[ibPentium];
    $3: if FCPUCache.Level2.Size<=128 then
          Result:=Result+[ibCeleron]
        else
          Result:=Result+[ibXeon];
    $6: Result:=Result+[ibPentium,ibMobile];
    $7: Result:=Result+[ibCeleron,ibMobile];
    $8: if (FFamily=$f) and (FModel=2) and (FStepping=4) then
          Result:=Result+[ibCeleron,ibMobile]
        else
          Result:=Result+[ibPentium];
    $A: Result:=Result+[ibCeleron];
    $9: Result:=Result+[ibPentium];
    $B: if FCPUCache.Level3.Size=0 then
          Result:=Result+[ibXeon]
        else
          Result:=Result+[ibXeon,ibMP];
    $C: Result:=Result+[ibXeon,ibMP];
    $E: Result:=Result+[ibPentium,ibMobile];
    $F: if (FFamily=$f) and (FModel=2) and (FStepping=7) then
          Result:=Result+[ibCeleron,ibMobile]
        else
          Result:=Result+[ibPentium,ibMobile];
    $11: Result:=Result+[ibMobile];
    $12: Result:=Result+[ibCeleron,ibM];
    $13: Result:=Result+[ibCeleron,ibMobile];
    $14: Result:=Result+[ibCeleron];
    $15: Result:=Result+[ibMobile];
    $16: Result:=Result+[ibPentium,ibM];
    $17: Result:=Result+[ibCeleron,ibMobile];
  end;
  if FCPP=2 then
    FIntelBrand:=FIntelBrand+[ibDuoCore];
  if (FFamily=15) then
    FIntelBrand:=FIntelBrand+[ibP4];
end;

function TMiTeC_CPU.GetPhysCount: Byte;
var
  i,n: Integer;
  CPUID: TCPUIDResult;
  APIC: Byte;
  LPP: Byte;
  LID: Byte;
  mask,b: Cardinal;
begin
  n:=GetCount;
  Result:=0;
  for i:=0 to n-1 do begin
    CPUID:=ExecuteCPUID(i,CPUID_STD_Signature,1);
    if (CPUID.EDX and (1 shl SFS_HTT))<>0 then begin
      APIC:=(CPUID.EBX and INITIAL_APIC_ID_BITS) shr 24;
      LPP:=(CPUID.EBX and NUM_LOGICAL_BITS) shr 16;
    end else begin
      APIC:=Byte(-1);
      LPP:=1;
    end;
    b:=1;
    mask:=PHY_ID_MASK;
    while (b<LPP) do begin
      b:=b*2;
      mask:=mask shl 1;
    end;
    LID:=APIC and not mask shl 24 shr 24;
    if LID=0 then
      Inc(Result);
  end;
  if Result=0 then
    Result:=n;
end;

{ TCPUCacheDetails }

procedure TCPUCacheDetails.Clear;
begin
  FTyp:=0;
  FDesc:='';
  FAssociativity:=caNone;
  FLinesize:=0;
  FSize:=0;
  ResetMemory(FDescriptors,SizeOf(FDescriptors));
  FWays:=0;
  FParts:=0;
  FLevel:=0;
  FShared:=0;
end;

constructor TCPUCacheDetails.Create;
begin
  inherited Create;
  FAssociativity:=caNone;
  FLinesize:=0;
  FSize:=0;
  ResetMemory(FDescriptors,SizeOf(FDescriptors));
end;

destructor TCPUCacheDetails.Destroy;
begin
  inherited;
end;

procedure TCPUCacheDetails.SetContent(AContent: TCacheDetails);
begin
  FTyp:=AContent.Typ;
  FDesc:=AContent.Desc;
  FAssociativity:=AContent.Associativity;
  FLinesize:=AContent.Linesize;
  FSize:=AContent.Size;
  FDescriptors:=AContent.Descriptors;
  FWays:=AContent.Ways;
  FParts:=AContent.Partitions;
  FLevel:=AContent.Level;
  FShared:=AContent.Shared;
end;

{ TCPUSegmentedCache }

procedure TCPUSegmentedCache.Clear;
begin
  FCode.Clear;
  FData.Clear;
  FUnified.Clear;
end;

constructor TCPUSegmentedCache.Create;
begin
  inherited Create;
  FCode:=TCPUCacheDetails.Create;
  FData:=TCPUCacheDetails.Create;
  FUnified:=TCPUCacheDetails.Create;
end;

destructor TCPUSegmentedCache.Destroy;
begin
  FCode.Free;
  FData.Free;
  FUnified.Free;
  inherited;
end;

procedure TCPUSegmentedCache.SetContent(ACode, AData, AUnified: TCacheDetails);
begin
  FCode.SetContent(ACode);
  FData.SetContent(AData);
  FUnified.SetContent(AUnified);
end;

{ TCPUCache }

procedure TCPUCache.AddData(AType: TProcessorCacheType; ALevel, AAssoc: Byte;
  ALineSize: Word; ASize: Cardinal);
var
  a: TCacheAssociativity;
begin
  case AAssoc of
    2: a:=ca2Way;
    4: a:=ca4Way;
    6: a:=ca6Way;
    8: a:=ca8Way;
    12: a:=ca12Way;
    16: a:=ca16Way;
    24: a:=ca24way;
    CACHE_FULLY_ASSOCIATIVE: a:=caFull;
    else a:=caNone;
  end;
  if AType=CacheTrace then begin
    FTrace.Ftyp:=1;
    Inc(FTrace.FShared);
    FTrace.FLevel:=1;
    FTrace.FSize:=ASize;
    FTrace.FLineSize:=ALineSize;
    FTrace.FAssociativity:=a;
    FTrace.FDesc:=Format('Trace %d KB, %s',[ASize,cAssociativityDescription[a]]);
  end else
  case ALevel of
    1: case AType of
         CacheUnified: begin
           FLevel1.FUnified.FTyp:=1;
           Inc(FLevel1.FUnified.FShared);
           FLevel1.FUnified.FLevel:=1;
           FLevel1.FUnified.FSize:=ASize;
           FLevel1.FUnified.FLineSize:=ALineSize;
           FLevel1.FUnified.FAssociativity:=a;
           FLevel1.FUnified.FDesc:=Format('Unified L1 %d KB, %s',[ASize,cAssociativityDescription[a]]);
         end;
        CacheInstruction: begin
          FLevel1.FCode.FTyp:=1;
          FLevel1.FCode.FLevel:=1;
          Inc(FLevel1.FCode.FShared);
          FLevel1.FCode.FSize:=ASize;
          FLevel1.FCode.FLineSize:=ALineSize;
          FLevel1.FCode.FAssociativity:=a;
          FLevel1.FCode.FDesc:=Format('Code L1 %d KB, %s',[ASize,cAssociativityDescription[a]]);
        end;
        CacheData: begin
          FLevel1.FData.FTyp:=1;
          FLevel1.FData.FLevel:=1;
          Inc(FLevel1.FData.FShared);
          FLevel1.FData.FSize:=ASize;
          FLevel1.FData.FLineSize:=ALineSize;
          FLevel1.FData.FAssociativity:=a;
          FLevel1.FData.FDesc:=Format('Data L1 %d KB, %s',[ASize,cAssociativityDescription[a]]);
        end;
    end;
    2: begin
      FLevel2.FTyp:=1;
      Inc(FLevel2.FShared);
      FLevel2.FLevel:=2;
      FLevel2.FSize:=ASize;
      FLevel2.FLineSize:=ALineSize;
      FLevel2.FAssociativity:=a;
      FLevel2.FDesc:=Format('Unified L2 %d KB, %s',[ASize,cAssociativityDescription[a]]);
    end;
    3: begin
      FLevel3.FTyp:=1;
      Inc(FLevel3.FShared);
      FLevel3.FLevel:=3;
      FLevel3.FSize:=ASize;
      FLevel3.FLineSize:=ALineSize;
      FLevel3.FAssociativity:=a;
      FLevel3.FDesc:=Format('Unified L3 %d KB, %s',[ASize,cAssociativityDescription[a]]);
    end;
  end;
end;

procedure TCPUCache.Clear;
begin
  FLevel1.Clear;
  FLevel2.Clear;
  FLevel3.Clear;
  FTrace.Clear;
end;

constructor TCPUCache.Create;
begin
  inherited Create;
  FLevel1:=TCPUSegmentedCache.Create;
  FLevel2:=TCPUCacheDetails.Create;
  FLevel3:=TCPUCacheDetails.Create;
  FTrace:=TCPUCacheDetails.Create;
end;

destructor TCPUCache.Destroy;
begin
  FLevel1.Free;
  FLevel2.Free;
  FLevel3.Free;
  FTrace.Free;
  inherited;
end;

procedure TCPUCache.SetContent(ALevel1Code, ALevel1data, ALevel1Unified,
  ALevel2, ALevel3, ATrace: TCacheDetails);
begin
  FLevel1.SetContent(ALevel1Code,ALevel1Data,ALevel1Unified);
  FLevel2.SetContent(Alevel2);
  FLevel3.SetContent(ALevel3);
  FTrace.SetContent(ATrace);
end;

{ TCPUFeatureSet }

destructor TCPUFeatureSet.Destroy;
begin
  Finalize(FAF);
  inherited;
end;

function TCPUFeatureSet.GetCount: Cardinal;
begin
  Result:=Length(FAF);
end;

function TCPUFeatureSet.GetFeature;
begin
  Finalize(Result);
  try
    Result:=FAF[Index]
  except
    ResetMemory(Result,SizeOf(Result));
  end;
end;

function TCPUFeatureSet.GetFeatureByFlag(Flag: Byte): TCPUFeature;
var
  i: Integer;
begin
  ResetMemory(Result,SizeOf(Result));
  for i:=0 to High(FAF) do
    if FAF[i].Index=Flag then begin
      Result:=FAF[i];
      Break;
    end;
end;

procedure TCPUFeatureSet.SetContent(AAF: TAvailableFeatures);
begin
  FAF:=AAF;
end;

{ TCPUFeatures }

constructor TCPUFeatures.Create;
begin
  inherited Create;
  FStd1:=TCPUFeatureSet.Create;
  FStd2:=TCPUFeatureSet.Create;
  FExt1:=TCPUFeatureSet.Create;
  FExt2:=TCPUFeatureSet.Create;
  FAPM:=TCPUFeatureSet.Create;
end;

destructor TCPUFeatures.Destroy;
begin
  FStd1.Free;
  FStd2.Free;
  FExt1.Free;
  FExt2.Free;
  FAPM.Free;
  inherited;
end;

procedure TCPUFeatures.SetContent;
begin
  FStd1.SetContent(AStd1);
  FStd2.SetContent(AStd2);
  FExt1.SetContent(AExt1);
  FExt2.SetContent(AExt2);
  FAPM.SetContent(AAPM);
end;

end.
