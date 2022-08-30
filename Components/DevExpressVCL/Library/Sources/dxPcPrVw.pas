{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPcPrVw;

interface

{$I cxVer.inc}

uses
  Graphics;

procedure dxShowPicturePreview(AGraphic: TGraphic);

implementation

uses
  Classes, SysUtils, Controls, ComCtrls, dxCore, cxClasses, dxPSUtl, dxPSRes, dxPSImgs,
  dxPSForm, dxExtCtrls, dxPSPrVw;

type
  TdxfmPicturePreview = class(TCustomdxPSForm)
  private
    FPictureBox: TdxPSImageScrollBox;
    FStatusBar: TdxPSStatusBar;
    function GetGraphic: TGraphic;
    function GetHasGraphic: Boolean;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure CreateControls;
    procedure UpdateControls;

    property Graphic: TGraphic read GetGraphic;
    property HasGraphic: Boolean read GetHasGraphic;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    procedure Execute(AGraphic: TGraphic);

    property PreviewBox: TdxPSImageScrollBox read FPictureBox;
    property StatusBar: TdxPSStatusBar read FStatusBar;
  end;

procedure dxShowPicturePreview(AGraphic: TGraphic);
begin
  with TdxfmPicturePreview.CreateNew(nil) do
  try
    Execute(AGraphic);
  finally
    Free;
  end;
end;

constructor TdxfmPicturePreview.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
begin
  inherited CreateNew(AOwner, Dummy);
  Caption := cxGetResourceString(@sdxFSPCaption);
  Constraints.MinHeight := 200;
  Constraints.MinWidth := 200;
  dxLoadIconFromResource(Icon, IDB_DXPSPREVIEW);
  CreateControls;
end;

procedure TdxfmPicturePreview.Execute(AGraphic: TGraphic);
begin
  PreviewBox.Picture.Assign(AGraphic);
  UpdateControls;
  ShowModal;
end;

procedure TdxfmPicturePreview.KeyPress(var Key: Char);
begin
  if Key = #27 then
    Close
  else
    inherited;
end;

procedure TdxfmPicturePreview.CreateControls;
begin
  FStatusBar := TdxPSStatusBar.Create(Self);
  FStatusBar.Parent := Self;
  FStatusBar.Align := alBottom;
  FStatusBar.Panels.Add.Width := 250;
  FStatusBar.Panels.Add.Width := -1;
  FStatusBar.Height := 20;

  FPictureBox := TdxPSImageScrollBox.Create(Self);
  FPictureBox.Parent := Self;
  FPictureBox.Align := alClient;
  FPictureBox.BuiltInMenuItemsVisibility := FPictureBox.BuiltInMenuItemsVisibility - [biiPreview];
end;

procedure TdxfmPicturePreview.UpdateControls;
begin
  Width := 400;
  Height := 400;

  if HasGraphic then
  begin
    ClientWidth := Graphic.Width + 10;
    ClientHeight := Graphic.Height + StatusBar.Height + 10;
    with dxPSUtl.GetDesktopWorkArea do
    begin
      if Width > Right - Left then Width := Right - Left;
      if Height > Bottom - Top then Height := Bottom - Top;
      Self.Left := (Right - Left - Width) div 2;
      Self.Top := (Bottom - Top - Height) div 2;
    end;
    StatusBar.Panels[0].Text := Format('%s: %d   %s: %d',
     [cxGetResourceString(@sdxWidth), Graphic.Width,
      cxGetResourceString(@sdxHeight), Graphic.Height]);
  end
  else
    PreviewBox.HintText := cxGetResourceString(@sdxNone);
end;

function TdxfmPicturePreview.GetGraphic: TGraphic;
begin
  Result := PreviewBox.Picture.Graphic;
end;

function TdxfmPicturePreview.GetHasGraphic: Boolean;
begin
  Result := PreviewBox.HasGraphic;
end;

end.

