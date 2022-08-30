{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit dxCameraDialog;

{$I cxVer.inc}

interface

uses
  Windows, Types, Graphics, Forms, Classes, Controls, StdCtrls, Menus, ExtCtrls,
  dxCore, cxClasses, cxGraphics, dxForms, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, dxCameraControl;

type
  TfrmCameraDialog = class(TdxForm)
    ccCamera: TdxCameraControl;
    btnCancel: TButton;
    btnAssign: TButton;
    btnSnapshot: TButton;
    pnlBottom: TPanel;
    procedure btnSnapshotClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure CameraStateChanged(Sender: TObject);
    procedure SetCaptions;
    procedure Initialize;
  end;

function dxShowCameraDialog(APicture: TPicture): Boolean; overload;
function dxShowCameraDialog(AGraphic: TGraphic): Boolean; overload;

implementation

{$R *.dfm}

uses
  cxEditConsts, cxGeometry;

type
  TdxCameraControlAccess = class(TdxCameraControl);

var
  dxgCameraDialogBounds: TRect;

function dxShowCameraDialog(APicture: TPicture): Boolean;
var
  ADialog: TfrmCameraDialog;
begin
  ADialog := TfrmCameraDialog.Create(Screen.ActiveForm);
  try
    Result := ADialog.ShowModal = mrOk;
    if Result then
      APicture.Assign(ADialog.ccCamera.CapturedBitmap);
  finally
    ADialog.Free;
  end;
end;

function dxShowCameraDialog(AGraphic: TGraphic): Boolean;
var
  ADialog: TfrmCameraDialog;
begin
  ADialog := TfrmCameraDialog.Create(Screen.ActiveForm);
  try
    Result := ADialog.ShowModal = mrOk;
    if Result then
      AGraphic.Assign(ADialog.ccCamera.CapturedBitmap);
  finally
    ADialog.Free;
  end;
end;

{ TfrmCameraDialog }

procedure TfrmCameraDialog.CameraStateChanged(Sender: TObject);
begin
  btnSnapshot.Enabled := TdxCameraControlAccess(ccCamera).State in [ccsRunning, ccsPaused];
end;

procedure TfrmCameraDialog.SetCaptions;
begin
  Caption := cxGetResourceString(@sdxCameraDialogCaption);
  btnCancel.Caption := cxGetResourceString(@sdxCameraDialogCancel);
  btnAssign.Caption := cxGetResourceString(@sdxCameraDialogAssign);
  btnSnapshot.Caption := cxGetResourceString(@sdxCameraDialogPause);
end;

procedure TfrmCameraDialog.Initialize;
begin
  if not cxRectIsEmpty(dxgCameraDialogBounds) then
  begin
    Position := poDesigned;
    BoundsRect := dxgCameraDialogBounds;
  end;

  btnSnapshot.Enabled := TdxCameraControlAccess(ccCamera).State in [ccsRunning, ccsPaused];
  TdxCameraControlAccess(ccCamera).OnStateChanged := CameraStateChanged;
end;

procedure TfrmCameraDialog.btnSnapshotClick(Sender: TObject);
var
  ACameraControl: TdxCameraControlAccess;
begin
  ACameraControl := TdxCameraControlAccess(ccCamera);
  case ACameraControl.State of
    ccsPaused:
      begin
        ACameraControl.Play;
        btnSnapshot.Caption := cxGetResourceString(@sdxCameraDialogPause);
        btnAssign.Enabled := False;
      end;
    ccsRunning:
      begin
        ACameraControl.Pause;
        btnSnapshot.Caption := cxGetResourceString(@sdxCameraDialogPlay);
        btnAssign.Enabled := True;
      end;
  end;
end;

procedure TfrmCameraDialog.FormCreate(Sender: TObject);
begin
  SetCaptions;
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  Initialize;
end;

procedure TfrmCameraDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  dxgCameraDialogBounds := BoundsRect;
end;

end.
