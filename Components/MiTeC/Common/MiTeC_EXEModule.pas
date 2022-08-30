{*******************************************************}
{                MiTeC Common Routines                  }
{             EXE Module & EXE Resources                }
{                                                       }
{           Copyright (c) 2015 Michal Mutl              }
{                                                       }
{*******************************************************}


{$INCLUDE Compilers.inc}

unit MiTeC_EXEModule;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes;
     {$ELSE}
     Windows, SysUtils, Classes;
     {$ENDIF}

type
  TExeModule=class
  private
    FHandle: HMODULE;
    FExeFile: string;
    FAutoLoad: Boolean;
    function GetIsLoaded: Boolean;
    procedure SetExeFile(const Value: string);
  public
    constructor Create(const AExeFile: string=''; const AAutoLoad: Boolean = True);
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure Load;
    procedure Unload;

    property Handle: HMODULE read FHandle;
    property ExeFile: string read FExeFile write SetExeFile;
    property IsLoaded: Boolean read GetIsLoaded;
    property AutoLoad: Boolean read FAutoLoad;
  end;

procedure AlterResource(const AExeFile, AResName: string; const AContent: Pointer; const ASize: Cardinal);
procedure ReadResource(const AExeFile, AResName: string; AStream: TStream);

implementation

procedure AlterResource(const AExeFile, AResName: string; const AContent: Pointer; const ASize: Cardinal);
var
  h: THandle;
begin
  h:=BeginUpdateResource(PChar(AExeFile),False);
  if h=0 then
    raise Exception.CreateFmt('Can not update resource of %s'#13#10#13#10'%s.',[AEXEFile,SysErrorMessage(GetLastError)]);
  try
    if not UpdateResource(h,RT_RCDATA,PChar(UpperCase(AResName)),LANG_NEUTRAL,AContent,ASize) then
      raise Exception.CreateFmt('Can not update resource %s of %s.'#13#10#13#10'%s',[AResName,AExeFile,SysErrorMessage(GetLastError)]);
  finally
    EndUpdateResource(h,False);
  end;
end;

procedure ReadResource(const AExeFile, AResName: string; AStream: TStream);
var
  rs: TResourceStream;
begin
  AStream.Size:=0;
  AStream.Position:=0;
  with TExeModule.Create(AExeFile) do
    try
      rs:=TResourceStream.Create(Handle,UpperCase(AResName),RT_RCDATA);
      try
        AStream.CopyFrom(rs,rs.Size);
      finally
        rs.Free;
      end;
    finally
      Free;
    end;
  AStream.Position:=0;
end;

{ TExeModule }

procedure TExeModule.AfterConstruction;
begin
  inherited;
  if FAutoLoad then
    Load;
end;

constructor TExeModule.Create(const AExeFile: string; const AAutoLoad: Boolean);
begin
  FExeFile:=AExeFile;
  if AAutoLoad then
    FAutoLoad:=AAutoLoad;
end;

destructor TExeModule.Destroy;
begin
  Unload;
  inherited;
end;

function TExeModule.GetIsLoaded: Boolean;
begin
  Result:=(FHandle<>0) and (FHandle<>INVALID_HANDLE_VALUE);
end;

procedure TExeModule.Load;
begin
  if IsLoaded then
    Exit;
  if FExeFile='' then
    raise Exception.Create('Executable file not specified');

  if not FileExists(FExeFile) then
    raise Exception.CreateFmt('%s is not found',[FExeFile]);

  FHandle:=LoadLibrary(PChar(FExeFile));
  if FHandle=0 then
    raise Exception.CreateFmt('Can not load %s.'#13#10#13#10'%s',[FExeFile,SysErrorMessage(GetLastError)]);
end;

procedure TExeModule.SetExeFile(const Value: string);
begin
  if SameText(FExeFile, Value) then
    Exit;
  FExeFile:=Value;
  Unload;
end;

procedure TExeModule.Unload;
begin
  if not IsLoaded then
    Exit;
  FreeLibrary(FHandle);
  FHandle:=0;
end;

end.
