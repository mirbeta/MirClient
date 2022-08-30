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

unit dxBrhDlg;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  UITypes,
{$ENDIF}
  Classes, Graphics, Controls, ExtCtrls, StdCtrls, Buttons, Menus,
  dxCore, dxPSForm, cxLookAndFeelPainters, cxControls,
  cxContainer, cxEdit, cxGroupBox, cxLabel, cxButtons, cxGraphics,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxColorComboBox, cxLookAndFeels, dxLayoutcxEditAdapters, dxLayoutContainer,
  dxLayoutControlAdapters, dxLayoutLookAndFeels, cxClasses, dxLayoutControl;

type
  TdxBrushDlg = class(TCustomdxPSForm)
    btnCancel: TcxButton;
    btnOK: TcxButton;
    ccbxColor: TcxColorComboBox;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLayoutGroup1: TdxLayoutGroup;
    lblColor: TdxLayoutItem;
    lblStyle: TdxLayoutItem;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    procedure ccbxColorChange(Sender: TObject);
    procedure ccbxStyleChange(Sender: TObject);
    procedure lblClick(Sender: TObject);
  private
    FBrush: TBrush;
    FcbxStyle: TcxCustomComboBox;
    FLockControls: Boolean;
    FModified: Boolean;

    procedure BrushChanged(Sender: TObject);
    procedure SetBrush(Value: TBrush);

    procedure CreateControls;
    procedure CreateBrush;
    procedure LoadStrings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;

    property Brush: TBrush read FBrush write SetBrush;
  end;

function AreEqualBrushes(const ABrushes: array of TBrush): Boolean;
function ChooseBrush(ABrush: TBrush): Boolean;

implementation

{$R *.DFM}

uses
  SysUtils, dxPSUtl, dxExtCtrls, dxPSRes;

function AreEqualBrushes(const ABrushes: array of TBrush): Boolean;
var
  Brush: TBrush;
  I: Integer;
begin
  Result := False;
  if High(ABrushes) - Low(ABrushes) > 1 then
  begin
    Brush := ABrushes[Low(ABrushes)];
    for I := Low(ABrushes) + 1 to High(ABrushes) do
      if (Brush.Style <> ABrushes[I].Style) or (Brush.Color <> ABrushes[I].Color) then
        Exit;
  end;
  Result := True;
end;

function ChooseBrush(ABrush: TBrush): Boolean;
begin
  Assert(ABrush <> nil);
  with TdxBrushDlg.Create(nil) do
  try
    Brush := ABrush;
    Result := Execute;
    if Result then ABrush.Assign(Brush);
  finally
    Free;
  end;
end;

{ TdxBrushDlg }

constructor TdxBrushDlg.Create(AOwner: TComponent);
begin
  inherited;
  CreateControls;
  CreateBrush;
end;

destructor TdxBrushDlg.Destroy;
begin
  FreeAndNil(FBrush);
  inherited;
end;

function TdxBrushDlg.Execute: Boolean;
begin
  LoadStrings;
  Result := (ShowModal = mrOk) and FModified;
end;

procedure TdxBrushDlg.ccbxStyleChange(Sender: TObject);
begin
  Brush.Style := TdxPSBrushStyleCombo(Sender).BrushStyle;
end;

procedure TdxBrushDlg.ccbxColorChange(Sender: TObject);
begin
  Brush.Color := TcxColorComboBox(Sender).ColorValue;
end;

procedure TdxBrushDlg.lblClick(Sender: TObject);
begin
  ActivateComboBoxControl(Self, TcxLabel(Sender).FocusControl);
end;

procedure TdxBrushDlg.CreateControls;
begin
  FcbxStyle := TdxPSBrushStyleCombo.Create(Self);
  TdxPSBrushStyleCombo(FcbxStyle).ShowStyleName := True;
  TdxPSBrushStyleCombo(FcbxStyle).Properties.OnChange := ccbxStyleChange;
  lblStyle.Control := FcbxStyle;
end;

procedure TdxBrushDlg.CreateBrush;
begin
  FBrush := TBrush.Create;
  FBrush.Color := clBlack;
  FBrush.Style := bsSolid;
  FBrush.OnChange := BrushChanged;
end;

procedure TdxBrushDlg.LoadStrings;
begin
  Caption := cxGetResourceString(@sdxBrushDlgCaption);
  lblColor.Caption := cxGetResourceString(@sdxColor);
  lblStyle.Caption := cxGetResourceString(@sdxStyle);
  btnOk.Caption := cxGetResourceString(@sdxBtnOK);
  btnCancel.Caption := cxGetResourceString(@sdxBtnCancel);
end;

procedure TdxBrushDlg.BrushChanged(Sender: TObject);
begin
  FModified := True;
  FLockControls := True;
  try
    ccbxColor.ColorValue := Brush.Color;
    TdxPSBrushStyleCombo(FcbxStyle).BrushColor := Brush.Color;
    TdxPSBrushStyleCombo(FcbxStyle).BrushStyle := Brush.Style;
  finally
    FLockControls := False;
  end;
end;

procedure TdxBrushDlg.SetBrush(Value: TBrush);
begin
  Brush.Assign(Value);
  FModified := False;
end;

end.
