{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library graphics classes          }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxActions;

{$I cxVer.inc}

interface

uses
  Windows, Types, SysUtils, Classes, Controls, ActnList, dxCore, cxClasses, ImgList;

type
  TdxBasicAction = class;

  { IdxActionValue }

  IdxActionValue = interface
  ['{D758AA66-953D-4DBE-9D4B-E9C79B5B5344}']
    function GetValue: Variant;
    procedure SetValue(const AValue: Variant);

    property Value: Variant read GetValue write SetValue;
  end;

  { IdxActionValueClient }

  IdxActionValueClient = interface
  ['{03AFE120-F929-4F90-B172-3A24D33A2BB5}']
    procedure ActionValueChanged(const AValue: Variant);
  end;

  { IdxActionColorValue }

  IdxActionColorValue = interface
  ['{64784163-746E-5661-6C43-6F6C6F720000}']
  end;

  { IdxActionFontNameValue }

  IdxActionFontNameValue = interface
  ['{64784163-746E-5661-6C46-6F6E744E616D}']
  end;

  { IdxActionGalleryInfo }

  IdxActionGalleryInfo = interface
  ['{5B03C13A-D181-4468-A46E-E001B1DDA705}']
    procedure Add(AActionIndex: Variant; const AGroupCaption, ACaption: string;
      const ADescription: string = ''; const AImageFileName: string = ''; AExternalImageIndex: Integer = -1);
    function GetGroups: TObject;
    procedure SetExternalImageList(AImageList: TCustomImageList);
  end;

  { IdxActionGalleryClient }

  IdxActionGalleryClient = interface(IdxActionValue)
  ['{DA02967D-130E-40E4-966A-FE5F5EB7D848}']
    procedure PopulateGalleryInfo(AInfo: IdxActionGalleryInfo);
  end;

  { IdxActionFontSizeValue }

  IdxActionFontSizeValue = interface
  ['{64784163-746E-5661-6C46-6F6E7453697A}']
  end;

  { TdxBasicActionAssignedValues }

  TdxBasicActionAssignedValues = class(TPersistent)
  strict private
    FCaption: Boolean;
    FHint: Boolean;
    FOwner: TdxBasicAction;

    procedure SetCaption(const AValue: Boolean);
    procedure SetHint(const AValue: Boolean);
  protected
    procedure Changed; virtual;
  public
    constructor Create(AOwner: TdxBasicAction); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; virtual;
  published
    property Caption: Boolean read FCaption write SetCaption default False;
    property Hint: Boolean read FHint write SetHint default False;
  end;

  { TdxBasicAction }

  TdxBasicActionClass = class of TdxBasicAction;
  TdxBasicAction = class abstract(TAction)
  strict private
    FAssignedValues: TdxBasicActionAssignedValues;
    FControl: TControl;
    FLockCount: Integer;
    FNeedControlFocus: Boolean;

    function GetCaption: string;
  {$IFNDEF DELPHI17}
    function GetClientCount: Integer;
  {$ENDIF}
    function GetHint: string;
    function IsCaptionStored: Boolean;
    function IsHintStored: Boolean;
    procedure ReadCaption(Reader: TReader);
    procedure ReadHint(Reader: TReader);
    procedure SetAssignedValues(const Value: TdxBasicActionAssignedValues);
    procedure WriteCaption(Writer: TWriter);
    procedure WriteHint(Writer: TWriter);
  protected
    FDefaultImageNameInIconLibrary: string;

    procedure DefineProperties(Filer: TFiler); override;
    function GetDefaultCaption: string; virtual; abstract;
    function GetDefaultHint: string; virtual; abstract;
    procedure SetCaption(const Value: string); {$IFDEF DELPHIXE3} override; {$ENDIF}
    procedure SetControl(Value: TControl); virtual;
    procedure SetHint(const Value: string); {$IFDEF DELPHIXE3} override; {$ENDIF}

    procedure AssignTo(Dest: TPersistent); override;
    procedure Change; override;
    procedure DoActionValueChanged(const AValue: Variant); virtual;
    function HandleShortCut: Boolean; override;
    function IsLoading: Boolean;
    procedure Loaded; override;
    function NeedControlFocus: Boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateCaption;
    procedure UpdateControl(Target: TObject); virtual;
    procedure UpdateHint;

    procedure DoResetState; virtual;
    procedure DoUpdateState; virtual;
    procedure UpdateState; virtual;

  {$IFNDEF DELPHI17}
    property ClientCount: Integer read GetClientCount;
  {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function Execute: Boolean; override;

    procedure ExecuteTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;

    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;
    function IsLocked: Boolean;

    property Control: TControl read FControl write SetControl;
  published
    property AssignedValues: TdxBasicActionAssignedValues read FAssignedValues write SetAssignedValues;
    property Caption: string read GetCaption write SetCaption stored IsCaptionStored;
    property Hint: string read GetHint write SetHint stored IsHintStored;
  end;

  { TdxCustomAction }

  TdxCustomAction = class(TdxBasicAction)
  protected
    FDefaultCaptionResString: Pointer;
    FDefaultHintResString: Pointer;

    function GetDefaultCaption: string; override;
    function GetDefaultHint: string; override;
  end;

implementation

{ TdxBasicActionAssignedValues }

constructor TdxBasicActionAssignedValues.Create(AOwner: TdxBasicAction);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxBasicActionAssignedValues.Assign(Source: TPersistent);
var
  ASource: TdxBasicActionAssignedValues;
begin
  if Source is TdxBasicActionAssignedValues then
  begin
    ASource := TdxBasicActionAssignedValues(Source);
    FHint := ASource.Hint;
    FCaption := ASource.Caption;
  end
  else
    inherited Assign(Source);
end;

procedure TdxBasicActionAssignedValues.Reset;
begin
  FCaption := False;
  FHint := False;
  Changed;
end;

procedure TdxBasicActionAssignedValues.Changed;
begin
  FOwner.Change;
end;

procedure TdxBasicActionAssignedValues.SetCaption(const AValue: Boolean);
begin
  if Caption <> AValue then
  begin
    FCaption := AValue;
    Changed
  end;
end;

procedure TdxBasicActionAssignedValues.SetHint(const AValue: Boolean);
begin
  if Hint <> AValue then
  begin
    FHint := AValue;
    Changed;
  end;
end;

{ TdxBasicAction }

constructor TdxBasicAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAssignedValues := TdxBasicActionAssignedValues.Create(Self);
  DisableIfNoHandler := True;
end;

destructor TdxBasicAction.Destroy;
begin
  Control := nil;
  FreeAndNil(FAssignedValues);
  inherited Destroy;
end;

procedure TdxBasicAction.AfterConstruction;
begin
  inherited AfterConstruction;
  UpdateCaption;
  UpdateHint;
end;

function TdxBasicAction.Execute: Boolean;
begin
  BeginUpdate;
  try
    Result := inherited Execute;
  finally
    EndUpdate;
  end;
end;

procedure TdxBasicAction.ExecuteTarget(Target: TObject);
begin
  // do nothing
end;

function TdxBasicAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := (Control <> nil) and (Target = Control);
end;

procedure TdxBasicAction.UpdateTarget(Target: TObject);
begin
  UpdateControl(Target);
  UpdateCaption;
  UpdateHint;
  UpdateState;
end;

procedure TdxBasicAction.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxBasicAction.CancelUpdate;
begin
  Dec(FLockCount);
end;

procedure TdxBasicAction.EndUpdate;
begin
  Dec(FLockCount);
  Change;
end;

function TdxBasicAction.IsLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

procedure TdxBasicAction.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Caption', ReadCaption, WriteCaption, IsCaptionStored and (Caption = ''));
  Filer.DefineProperty('Hint', ReadHint, WriteHint, IsHintStored and (Hint = ''));
end;

procedure TdxBasicAction.SetCaption(const Value: string);
begin
  if Caption <> Value then
  begin
    if not IsLoading then
      AssignedValues.Caption := True;
  {$IFDEF DELPHIXE3}
    inherited SetCaption(Value);
  {$ELSE}
    inherited Caption := Value;
  {$ENDIF}
  end;
end;

procedure TdxBasicAction.SetControl(Value: TControl);
begin
  if FControl <> Value then
  begin
    if FControl <> nil then
      FControl.RemoveFreeNotification(Self);
    FControl := Value;
    if FControl <> nil then
      FControl.FreeNotification(Self);
    UpdateState;
  end;
end;

procedure TdxBasicAction.SetHint(const Value: string);
begin
  if Hint <> Value then
  begin
    if not IsLoading then
      AssignedValues.Hint := True;
  {$IFDEF DELPHIXE3}
    inherited SetHint(Value);
  {$ELSE}
    inherited Hint := Value;
  {$ENDIF}
  end;
end;

procedure TdxBasicAction.AssignTo(Dest: TPersistent);
var
  ADest: TdxBasicAction;
begin
  if Dest is TdxBasicAction then
  begin
    ADest := TdxBasicAction(Dest);
    AssignedValues.Assign(ADest.AssignedValues);
  end;
  inherited AssignTo(Dest);
end;

procedure TdxBasicAction.Change;
begin
  if not IsLocked then
  begin
    inherited Change;
    UpdateCaption;
    UpdateHint;
  end;
end;

procedure TdxBasicAction.DoActionValueChanged(const AValue: Variant);
var
  AIntf: IdxActionValueClient;
  I: Integer;
begin
  for I := 0 to ClientCount - 1 do
  begin
    if Supports(Clients[I], IdxActionValueClient, AIntf) then
      AIntf.ActionValueChanged(AValue);
  end;
end;

function TdxBasicAction.HandleShortCut: Boolean;
begin
  FNeedControlFocus := True;
  try
    Result := inherited HandleShortCut;
  finally
    FNeedControlFocus := False;
  end;
end;

function TdxBasicAction.IsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

procedure TdxBasicAction.Loaded;
begin
  inherited Loaded;
  UpdateCaption;
  UpdateHint;
end;

function TdxBasicAction.NeedControlFocus: Boolean;
begin
  Result := FNeedControlFocus;
end;

procedure TdxBasicAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Control) then
    FControl := nil;
end;

procedure TdxBasicAction.UpdateCaption;
begin
  if not IsLoading and not AssignedValues.Caption then
  begin
    BeginUpdate;
    try
    {$IFDEF DELPHIXE3}
      inherited SetCaption(GetDefaultCaption);
    {$ELSE}
      inherited Caption := GetDefaultCaption;
    {$ENDIF}
    finally
      CancelUpdate;
    end;
  end;
end;

procedure TdxBasicAction.UpdateControl(Target: TObject);
begin
  // do nothing
end;

procedure TdxBasicAction.UpdateHint;
begin
  if not IsLoading and not AssignedValues.Hint then
  begin
    BeginUpdate;
    try
    {$IFDEF DELPHIXE3}
      inherited SetHint(GetDefaultHint);
    {$ELSE}
      inherited Hint := GetDefaultHint;
    {$ENDIF}
    finally
      CancelUpdate;
    end;
  end;
end;

procedure TdxBasicAction.DoResetState;
begin
  // do nothing
end;

procedure TdxBasicAction.DoUpdateState;
begin
  // do nothing
end;

procedure TdxBasicAction.UpdateState;
begin
  if Control <> nil then
    DoUpdateState
  else
    DoResetState;
end;

function TdxBasicAction.GetCaption: string;
begin
  Result := inherited Caption;
end;

{$IFNDEF DELPHI17}
function TdxBasicAction.GetClientCount: Integer;
begin
  Result := FClients.Count;
end;
{$ENDIF}

function TdxBasicAction.GetHint: string;
begin
  Result := inherited Hint;
end;

function TdxBasicAction.IsCaptionStored: Boolean;
begin
  Result := AssignedValues.Caption;
end;

function TdxBasicAction.IsHintStored: Boolean;
begin
  Result := AssignedValues.Hint;
end;

procedure TdxBasicAction.ReadCaption(Reader: TReader);
begin
  Caption := Reader.ReadString;
end;

procedure TdxBasicAction.ReadHint(Reader: TReader);
begin
  Hint := Reader.ReadString;
end;

procedure TdxBasicAction.SetAssignedValues(const Value: TdxBasicActionAssignedValues);
begin
  FAssignedValues.Assign(Value);
end;

procedure TdxBasicAction.WriteCaption(Writer: TWriter);
begin
  Writer.WriteString(Caption);
end;

procedure TdxBasicAction.WriteHint(Writer: TWriter);
begin
  Writer.WriteString(Hint);
end;

{ TdxCustomAction }

function TdxCustomAction.GetDefaultCaption: string;
begin
  Result := cxGetResourceString(FDefaultCaptionResString);
end;

function TdxCustomAction.GetDefaultHint: string;
begin
  Result := cxGetResourceString(FDefaultHintResString);
end;

end.
