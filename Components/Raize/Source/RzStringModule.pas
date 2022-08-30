{===============================================================================
  RzStringModule Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzStringModuleCreator
    This unit implements the TRzStringModuleCreator and the TRzOTAFile classes. 
    These classes are used in the Raize String List editor to implement the Code
    Editor button in Delphi 5 and higher.


  Modification History
  ------------------------------------------------------------------------------
  4.2    (29 May 2007)
    * Updated to reflect changes in recent Delphi IDEs.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * No changes.
===============================================================================}

{$I RzComps.inc}

unit RzStringModule;

interface

uses
  SysUtils, 
  Classes, 
  DesignConst, 
  ToolsAPI, 
  IStreams, 
  StFilSys, 
  TypInfo;


{================================}
{== Code Editor Button Classes ==}
{================================}

type
  TRzStringsModuleCreator = class( TInterfacedObject, IOTACreator, IOTAModuleCreator )
  private
    FFileName: string;
    FStream: TStringStream;
    FAge: TDateTime;
  public
    constructor Create( const FileName: string; Stream: TStringStream; Age: TDateTime );
    destructor Destroy; override;
    { IOTACreator }
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    { IOTAModuleCreator }
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile( const FormIdent, AncestorIdent: string ): IOTAFile;
    function NewImplSource( const ModuleIdent, FormIdent, AncestorIdent: string ): IOTAFile;
    function NewIntfSource( const ModuleIdent, FormIdent, AncestorIdent: string ): IOTAFile;
    procedure FormCreated( const FormEditor: IOTAFormEditor );
  end;

  TRzOTAFile = class( TInterfacedObject, IOTAFile )
  private
    FSource: string;
    FAge: TDateTime;
  public
    constructor Create( const ASource: string; AAge: TDateTime );
    { IOTAFile }
    function GetSource: string;
    function GetAge: TDateTime;
  end;

implementation


{=====================================}
{== TRzStringsModuleCreator Methods ==}
{=====================================}

constructor TRzStringsModuleCreator.Create( const FileName: string; Stream: TStringStream; 
                                            Age: TDateTime );
begin
  inherited Create;
  FFileName := FileName;
  FStream := Stream;
  FAge := Age;
end;

destructor TRzStringsModuleCreator.Destroy;
begin
  FStream.Free;
  inherited;
end;

procedure TRzStringsModuleCreator.FormCreated( const FormEditor: IOTAFormEditor );
begin
  { Do Nothing }
end;

function TRzStringsModuleCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TRzStringsModuleCreator.GetCreatorType: string;
begin
  Result := sText;
end;

function TRzStringsModuleCreator.GetExisting: Boolean;
begin
  Result := True;
end;

function TRzStringsModuleCreator.GetFileSystem: string;
begin
  Result := sTStringsFileSystem;
end;

function TRzStringsModuleCreator.GetFormName: string;
begin
  Result := '';
end;

function TRzStringsModuleCreator.GetImplFileName: string;
begin
  Result := FFileName;
end;

function TRzStringsModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TRzStringsModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TRzStringsModuleCreator.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TRzStringsModuleCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

function TRzStringsModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TRzStringsModuleCreator.GetUnnamed: Boolean;
begin
  Result := False;
end;

function TRzStringsModuleCreator.NewFormFile( const FormIdent, AncestorIdent: string ): IOTAFile;
begin
  Result := nil;
end;

function TRzStringsModuleCreator.NewImplSource( const ModuleIdent, FormIdent, AncestorIdent: string ): IOTAFile;
begin
  Result := TRzOTAFile.Create( FStream.DataString, FAge );
end;

function TRzStringsModuleCreator.NewIntfSource( const ModuleIdent, FormIdent, AncestorIdent: string ): IOTAFile;
begin
  Result := nil;
end;


{========================}
{== TRzOTAFile Methods ==}
{========================}

constructor TRzOTAFile.Create( const ASource: string; AAge: TDateTime );
begin
  inherited Create;
  FSource := ASource;
  FAge := AAge;
end;

function TRzOTAFile.GetAge: TDateTime;
begin
  Result := FAge;
end;

function TRzOTAFile.GetSource: string;
begin
  Result := FSource;
end;


end.
