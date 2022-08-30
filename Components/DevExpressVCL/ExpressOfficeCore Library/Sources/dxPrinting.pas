{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressOfficeCore Library classes                        }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSOFFICECORE LIBRARY AND ALL     }
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

unit dxPrinting;

{$I cxVer.inc}

interface

uses
  Types, Classes, Controls, Generics.Defaults, Generics.Collections, dxActions;

type
  { IdxPrinting }

  IdxPrinting = interface
  ['{2307ACEC-0204-4116-89A9-041BF8F4E05A}']
    function CanBuildReport(AComponent: TComponent): Boolean;
    function PageSetup: Boolean;
    procedure Preview(Modal: Boolean = True);
    function Print(AShowDialog: Boolean): Boolean; overload;
    procedure Print(const APageIndexes: array of Integer); overload;
  end;

  { TdxPrintingRepository }

  TdxPrintingRepository = class
  strict private
    FList: TList<IdxPrinting>;
    function Find(AComponent: TComponent; out APrinting: IdxPrinting): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function CanBuildReport(AComponent: TComponent): Boolean;
    procedure PageSetupReport(AComponent: TComponent);
    procedure PreviewReport(AComponent: TComponent);
    procedure PrintReport(AComponent: TComponent); overload;
    procedure PrintReport(AComponent: TComponent; const APageIndexes: array of Integer); overload;
    procedure Register(APrinting: IdxPrinting);
    procedure Unregister(APrinting: IdxPrinting);
  end;

  { TdxCustomPrintingAction }

  TdxCustomPrintingAction = class(TdxCustomAction)
  strict private
    function GetControl: TWinControl;
  protected
    procedure SetControl(Value: TWinControl); reintroduce;
    //
    procedure DoExecute(AControl: TWinControl); virtual; abstract;
    procedure DoUpdateState; override;
    function IsEnabled: Boolean; virtual;
    function GetControlClass: TWinControlClass; virtual; abstract;
    procedure DoResetState; override;
    procedure UpdateControl(Target: TObject); override;
    //
    property ControlClass: TWinControlClass read GetControlClass;
    property Control: TWinControl read GetControl write SetControl;
  public
    procedure ExecuteTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
  end;

  { TdxCustomShowPageSetupFormAction }

  TdxCustomShowPageSetupFormAction = class(TdxCustomPrintingAction)
  protected
    procedure DoExecute(AControl: TWinControl); override;
  end;

  { TdxCustomShowPrintFormAction }

  TdxCustomShowPrintFormAction = class(TdxCustomPrintingAction)
  protected
    procedure DoExecute(AControl: TWinControl); override;
  end;

  { TdxCustomShowPrintPreviewFormAction }

  TdxCustomShowPrintPreviewFormAction = class(TdxCustomPrintingAction)
  protected
    procedure DoExecute(AControl: TWinControl); override;
  end;

function dxPrintingRepository: TdxPrintingRepository;

implementation

uses
  SysUtils;

var
  FdxPrintingRepository: TdxPrintingRepository;

function dxPrintingRepository: TdxPrintingRepository;
begin
  if FdxPrintingRepository = nil then
    FdxPrintingRepository := TdxPrintingRepository.Create;
  Result := FdxPrintingRepository;
end;

{ TdxPrintingRepository }

constructor TdxPrintingRepository.Create;
begin
  inherited Create;
  FList := TList<IdxPrinting>.Create;
end;

destructor TdxPrintingRepository.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TdxPrintingRepository.CanBuildReport(AComponent: TComponent): Boolean;
var
  APrinting: IdxPrinting;
begin
  Result := Find(AComponent, APrinting);
end;

procedure TdxPrintingRepository.PageSetupReport(AComponent: TComponent);
var
  APrinting: IdxPrinting;
begin
  if Find(AComponent, APrinting) then
    APrinting.PageSetup;
end;

procedure TdxPrintingRepository.PreviewReport(AComponent: TComponent);
var
  APrinting: IdxPrinting;
begin
  if Find(AComponent, APrinting) then
    APrinting.Preview;
end;

procedure TdxPrintingRepository.PrintReport(AComponent: TComponent);
var
  APrinting: IdxPrinting;
begin
  if Find(AComponent, APrinting) then
    APrinting.Print(True);
end;

procedure TdxPrintingRepository.PrintReport(AComponent: TComponent; const APageIndexes: array of Integer);
var
  APrinting: IdxPrinting;
begin
  if Find(AComponent, APrinting) then
    APrinting.Print(APageIndexes);
end;

procedure TdxPrintingRepository.Register(APrinting: IdxPrinting);
begin
  if not FList.Contains(APrinting) then
    FList.Add(APrinting);
end;

procedure TdxPrintingRepository.Unregister(APrinting: IdxPrinting);
begin
  FList.Remove(APrinting);
end;

function TdxPrintingRepository.Find(AComponent: TComponent; out APrinting: IdxPrinting): Boolean;
var
  AIntf: IdxPrinting;
begin
  Result := False;
  APrinting := nil;
  for AIntf in FList do
    if AIntf.CanBuildReport(AComponent) then
    begin
      APrinting := AIntf;
      Exit(True);
    end;
end;

{ TdxCustomPrintingAction }

procedure TdxCustomPrintingAction.ExecuteTarget(Target: TObject);
begin
  DoExecute(TWinControl(Target));
end;

function TdxCustomPrintingAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := (inherited HandlesTarget(Target) or (Target is ControlClass)) and
    (not NeedControlFocus or TWinControl(Target).Focused);
end;

procedure TdxCustomPrintingAction.SetControl(Value: TWinControl);
begin
  inherited SetControl(Value);
end;

procedure TdxCustomPrintingAction.DoUpdateState;
begin
  inherited DoUpdateState;
  Enabled := IsEnabled;
end;

function TdxCustomPrintingAction.IsEnabled: Boolean;
begin
  Result := dxPrintingRepository.CanBuildReport(Control) and Control.CanFocus;
end;

procedure TdxCustomPrintingAction.DoResetState;
begin
  inherited DoResetState;
  Enabled := False;
end;

procedure TdxCustomPrintingAction.UpdateControl(Target: TObject);
begin
  if Target is ControlClass then
    Control := TWinControl(Target);
end;

function TdxCustomPrintingAction.GetControl: TWinControl;
begin
  Result := TWinControl(inherited Control);
end;

{ TdxCustomShowPageSetupFormAction }

procedure TdxCustomShowPageSetupFormAction.DoExecute(AControl: TWinControl);
begin
  dxPrintingRepository.PageSetupReport(AControl);
end;

{ TdxCustomShowPrintFormAction }

procedure TdxCustomShowPrintFormAction.DoExecute(AControl: TWinControl);
begin
  dxPrintingRepository.PrintReport(AControl);
end;

{ TdxCustomShowPrintPreviewFormAction }

procedure TdxCustomShowPrintPreviewFormAction.DoExecute(AControl: TWinControl);
begin
  dxPrintingRepository.PreviewReport(AControl);
end;

initialization

finalization
  FreeAndNil(FdxPrintingRepository);

end.

