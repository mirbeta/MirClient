{*******************************************************}
{       MiTeC System Information Component Suite        }
{                Definitions & Types                    }
{                 version 13.4.0                        }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.Inc}


unit MSI_Defs;

interface

const
  cMajorVersion = '13';
  cMinorVersion = '4';
  cFixVersion = '0';
  cVersion = cMajorVersion+'.'+cMinorVersion+'.'+cFixVersion;
  cCompName = 'MiTeC System Information Management Suite';
  cCopyright = 'Copyright (c) 1997-2019, Michal Mutl';
  cWWW = 'http://www.mitec.cz/';
  cEmail = 'mailto:michal.mutl@mitec.cz';

  ARRAY_PRE_SIZE = 100;

type
  TScanObject = (soCPU, soMachine, soDevices, soDisplay, soMonitor, soNetwork, soMedia,
                soMemory, soStorage, soUSB, soEngines, soAPM, soDisk, soOS,
                soPrinters, soSoftware, soStartup, soProcesses, soMSProduct, soBT,
                soEventLog, soAD, soSecurity, soWIFI, soNetCreds, soUSBHistory,soWLANC,
                soFirewall);

  TScanObjects = set of TScanObject;

const
  soAll = [soCPU, soMachine, soDevices, soDisplay, soMonitor, soNetwork, soMedia,
           soMemory, soStorage, soUSB, soEngines, soAPM, soDisk, soOS,
           soPrinters, soSoftware, soStartup, soProcesses, soMSProduct, soBT, soEventLog,
           soAD, soSecurity, soWIFI, soNetCreds, soUSBHistory, soWLANC, soFirewall
];

  soBasic = [soCPU, soMachine, soDevices, soDisplay, soMonitor, soNetwork, soMedia,
           soMemory, soStorage, soUSB, soEngines, soAPM, soDisk, soOS,
           soPrinters, soSoftware, soStartup, soProcesses, soMSProduct, soBT, soSecurity,
           soWIFI, soFirewall];

  soForensic = [soNetCreds, soUSBHistory, soWLANC];

{$IFDEF TRIAL}function RunFromIDE: Boolean;{$ENDIF}
function ScanObjectsAsInt(A: TScanObjects): Cardinal;
function IntAsScanObjects(A: Cardinal): TScanObjects;

implementation

{$IFDEF TRIAL}
uses {$IFDEF RAD9PLUS}
     WinApi.Windows, System.SysUtils, WinApi.TlHelp32, WinApi.PsAPI,
     {$ELSE}
     Windows, SysUtils, {$IFDEF FPC}JwaTlHelp32, JwaPsAPI{$ELSE}TlHelp32, PsApi{$ENDIF},
     {$ENDIF}
     MiTeC_Routines;

function RunFromIDE: Boolean;
const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;
var
  ps,ph: THandle;
  pe32: TProcessEntry32;
  ok: Boolean;
  Buf: array[0..MAX_PATH] of char;
  s: string;
  vi: TVersionInfo;
begin
  Result:=False;
  ps:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  if (ps<>INVALID_HANDLE_VALUE) then
    try
      pe32.dwSize:=sizeof(TPROCESSENTRY32);
      ok:=Process32First(ps,pe32);
      while ok do begin
        if pe32.th32ProcessID=GetCurrentProcessId then begin
          ph:=OpenProcess(PROCESS_ALL_ACCESS,False,pe32.th32ParentProcessID);
          if ph=0 then
            ph:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ or PROCESS_VM_OPERATION,False,pe32.th32ParentProcessID);
          if ph=0 then
            ph:=OpenProcess(PROCESS_QUERY_INFORMATION,False,pe32.th32ParentProcessID);
          if ph=0 then
            ph:=OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION,False,pe32.th32ParentProcessID);
          if ph>0 then begin
            ResetMemory(Buf,SizeOf(Buf));
            if GetModuleFileNameEx(ph,0,@Buf,SizeOf(Buf))>0 then begin
              s:=Buf;
              SetLength(s,StrLen(PChar(s)));
              s:=StringReplace(s,'\??\','',[]);
            end;
            CloseHandle(ph);
          end;
          if FileExists(s) then begin
            GetFileVerInfo(s,vi);
            vi.CompanyName:=lowercase(vi.CompanyName);
            s:=Lowercase(ExtractFilename(s));
            Result:=((Pos(s,'delphi.exe,bds.exe')>0) and
                    ((Pos('embarcadero',vi.CompanyName)>0) or (Pos('codegear',vi.CompanyName)>0) or (Pos('borland',vi.CompanyName)>0)))
                    or
                    (Pos(s,'startlazarus.exe,lazarus.exe,gdb.exe')>0);

          end;
          Break;
        end;
        ok:=Process32Next(ps,pe32);
      end;
    finally
      CloseHandle(ps);
    end;
end;
{$ENDIF}

function ScanObjectsAsInt;
var
  i: TScanObject;
begin
  Result:=0;
  for i:=Low(TScanObject) to High(TScanObject) do
    if i in A then
      Result:=Result or (1 shl Integer(i));
end;

function IntAsScanObjects;
var
  i: TScanObject;
begin
  Result:=[];
  for i:=Low(TScanObject) to High(TScanObject) do
    if (A and (1 shl Integer(i)))<>0 then
      Result:=Result+[i];
end;

end.










