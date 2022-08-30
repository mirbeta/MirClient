{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCore Library                                      }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCORE LIBRARY AND ALL           }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxTrialDesignTimeHelper;

{$I cxVer.inc}

interface

uses
  dxCore, dxCoreClasses, ToolsAPI;

type

  { TdxTrialDesignTimeCustomHelper }

  TdxTrialDesignTimeHelper = class(TcxIUnknownObject, IOTANotifier, IOTAIDENotifier)
  private
    FServicesNotifierID: Integer;
    // IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    // IOTAIDENotifier
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
  protected
    function CheckCanCompileProject(const Project: IOTAProject): Boolean;
    function ShowWarningDialog(var AOptionValue: Boolean): Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

uses
{$IFDEF DELPHI16}
  CommonOptionStrs,
{$ENDIF}
  SysUtils, dxTrialDesignTimeHelperDialog, Controls;

const
{$IFNDEF DELPHI16}
	sUsePackages = 'UsePackages';
{$ENDIF}

  sdxTrialHelperRegistryRuntimePackagesErrorMessage = 'TrialVersionRuntimePackagesErrorMessage';
  sdxTrialHelperRegistryPath = 'Software\Developer Express\ExpressCore';

  sdxTrialHelperDialogDescription =
    'To compile your projects with an evaluation version of DevExpress VCL controls, you must enable the "Link with ' +
    'runtime packages" option in the project settings (Project | Options... > Packages > Runtime Packages).';

  sdxTrialHelperDialogCheckBoxCaption = 'Enable the option for this project';

{ TdxTrialDesignTimeHelper }

constructor TdxTrialDesignTimeHelper.Create;
var
  AServices: IOTAServices;
begin
  if Supports(BorlandIDEServices, IOTAServices, AServices) then
    FServicesNotifierID := AServices.AddNotifier(Self)
  else
    FServicesNotifierID := -1;
end;

destructor TdxTrialDesignTimeHelper.Destroy;
var
  AServices: IOTAServices;
begin
  if FServicesNotifierID >= 0 then
  begin
    if Supports(BorlandIDEServices, IOTAServices, AServices) then
      AServices.RemoveNotifier(FServicesNotifierID);
    FServicesNotifierID := -1;
  end;
  inherited Destroy;
end;

function TdxTrialDesignTimeHelper.CheckCanCompileProject(const Project: IOTAProject): Boolean;
var
  ABuildConfig: IOTABuildConfiguration;
  AProjectConfig: IOTAProjectOptionsConfigurations;
  AValue: Boolean;
begin
  Result := True;
  if Supports(Project.ProjectOptions, IOTAProjectOptionsConfigurations, AProjectConfig) then
  begin
    ABuildConfig := AProjectConfig.ActiveConfiguration;
    if (ABuildConfig <> nil) and not ABuildConfig.GetBoolean(sUsePackages) then
    begin
      if ShowWarningDialog(AValue) and AValue then
        ABuildConfig.SetBoolean(sUsePackages, AValue)
      else
        Result := False;
    end;
  end;
end;

function TdxTrialDesignTimeHelper.ShowWarningDialog(var AOptionValue: Boolean): Boolean;
var
  ADialog: TfrmTrialDesignTimeHelperDialog;
begin
  ADialog := TfrmTrialDesignTimeHelperDialog.Create(nil);
  try
    ADialog.lbDetails.Caption := sdxTrialHelperDialogDescription;
    ADialog.cbCheckbox.Caption := sdxTrialHelperDialogCheckBoxCaption;
    ADialog.cbCheckbox.Checked := AOptionValue;
    Result := ADialog.ShowModal = mrOk;
    if Result then
      AOptionValue := ADialog.cbCheckbox.Checked;
  finally
    ADialog.Free;
  end;
end;

procedure TdxTrialDesignTimeHelper.AfterSave;
begin
  // do nothing
end;

procedure TdxTrialDesignTimeHelper.BeforeSave;
begin
  // do nothing
end;

procedure TdxTrialDesignTimeHelper.Destroyed;
begin
  // do nothing
end;

procedure TdxTrialDesignTimeHelper.Modified;
begin
  // do nothing
end;

procedure TdxTrialDesignTimeHelper.AfterCompile(Succeeded: Boolean);
begin
  // do nothing
end;

procedure TdxTrialDesignTimeHelper.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
  if cxIsDXComponentsUsed then
    Cancel := not CheckCanCompileProject(Project);
end;

procedure TdxTrialDesignTimeHelper.FileNotification(
  NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
begin
  // do nothing
end;

end.
