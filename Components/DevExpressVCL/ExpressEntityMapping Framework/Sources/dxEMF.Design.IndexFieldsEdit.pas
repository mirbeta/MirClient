{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEntityMapping Framework                           }
{                                                                    }
{           Copyright (c) 2016-2019 Developer Express Inc.           }
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
{   (DCU, OBJ, DLL, DPU, SO, ETC.) ARE CONFIDENTIAL AND PROPRIETARY  }
{   TRADE SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER }
{   IS LICENSED TO DISTRIBUTE THE EXPRESSENTITYMAPPING FRAMEWORK     }
{   AS PART OF AN EXECUTABLE PROGRAM ONLY.                           }
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

unit dxEMF.Design.IndexFieldsEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, Dialogs, Controls, StdCtrls, Buttons;

type
  TIndexFieldsEditDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    SrcList: TListBox;
    DstList: TListBox;
    SrcLabel: TLabel;
    DstLabel: TLabel;
    IncludeBtn: TSpeedButton;
    IncAllBtn: TSpeedButton;
    ExcludeBtn: TSpeedButton;
    ExAllBtn: TSpeedButton;
    DownFldBtn: TSpeedButton;
    UpFldBtn: TSpeedButton;
    procedure IncludeBtnClick(Sender: TObject);
    procedure ExcludeBtnClick(Sender: TObject);
    procedure IncAllBtnClick(Sender: TObject);
    procedure ExcAllBtnClick(Sender: TObject);
    procedure DownFldBtnClick(Sender: TObject);
    procedure UpFldBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetIndexFieldNames: string;
    procedure SetIndexFieldNames(const Value: string);
    procedure UpdateSrcList;
    function GetSourceFields: TStrings; inline;
  protected
    procedure MoveSelected(AList: TCustomListBox; AItems: TStrings);
    procedure SetItem(AList: TListBox; AIndex: Integer);
    function GetFirstSelection(AList: TCustomListBox): Integer;
    procedure SetButtons;
  public
    property SourceFields: TStrings read GetSourceFields;
    property IndexFieldNames: string read GetIndexFieldNames write SetIndexFieldNames;
  end;

var
  IndexFieldsEditDlg: TIndexFieldsEditDlg;

implementation

{$R *.dfm}

{ TIndexFieldsEditDlg }

procedure TIndexFieldsEditDlg.IncludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(SrcList);
  MoveSelected(SrcList, DstList.Items);
  SetItem(SrcList, Index);
end;

procedure TIndexFieldsEditDlg.ExcludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(DstList);
  MoveSelected(DstList, SrcList.Items);
  SetItem(DstList, Index);
end;

procedure TIndexFieldsEditDlg.FormCreate(Sender: TObject);
begin
  DstList.Items.Delimiter := ';';
end;

procedure TIndexFieldsEditDlg.IncAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to SrcList.Items.Count - 1 do
    DstList.Items.AddObject(SrcList.Items[I],
      SrcList.Items.Objects[I]);
  SrcList.Items.Clear;
  SetItem(SrcList, 0);
end;

procedure TIndexFieldsEditDlg.DownFldBtnClick(Sender: TObject);
var
  AIndex: Integer;
begin
  AIndex := GetFirstSelection(DstList);
  if (AIndex >= 0) and (AIndex < DstList.Items.Count - 1) then
  begin
    DstList.Items.Exchange(AIndex, AIndex + 1);
    SetItem(DstList, AIndex + 1);
  end;
end;

procedure TIndexFieldsEditDlg.ExcAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to DstList.Items.Count - 1 do
    SrcList.Items.AddObject(DstList.Items[I], DstList.Items.Objects[I]);
  DstList.Items.Clear;
  SetItem(DstList, 0);
end;

procedure TIndexFieldsEditDlg.MoveSelected(AList: TCustomListBox; AItems: TStrings);
var
  I: Integer;
begin
  for I := AList.Items.Count - 1 downto 0 do
    if AList.Selected[I] then
    begin
      AItems.AddObject(AList.Items[I], AList.Items.Objects[I]);
      AList.Items.Delete(I);
    end;
end;

procedure TIndexFieldsEditDlg.SetButtons;
var
  ASrcEmpty, ADstEmpty: Boolean;
begin
  ASrcEmpty := SrcList.Items.Count = 0;
  ADstEmpty := DstList.Items.Count = 0;
  IncludeBtn.Enabled := not ASrcEmpty;
  IncAllBtn.Enabled := not ASrcEmpty;
  ExcludeBtn.Enabled := not ADstEmpty;
  ExAllBtn.Enabled := not ADstEmpty;
  UpFldBtn.Enabled := DstList.Items.Count > 1;
  DownFldBtn.Enabled := UpFldBtn.Enabled;
end;

function TIndexFieldsEditDlg.GetFirstSelection(AList: TCustomListBox): Integer;
begin
  for Result := 0 to AList.Items.Count - 1 do
    if AList.Selected[Result] then Exit;
  Result := LB_ERR;
end;

function TIndexFieldsEditDlg.GetIndexFieldNames: string;
begin
  Result := DstList.Items.DelimitedText;
end;

function TIndexFieldsEditDlg.GetSourceFields: TStrings;
begin
  Result := SrcList.Items;
end;

procedure TIndexFieldsEditDlg.SetIndexFieldNames(const Value: string);
begin
  DstList.Items.DelimitedText := Value;
  UpdateSrcList;
end;

procedure TIndexFieldsEditDlg.SetItem(AList: TListBox; AIndex: Integer);
var
  MaxIndex: Integer;
begin
  with AList do
  begin
    SetFocus;
    MaxIndex := AList.Items.Count - 1;
    if AIndex = LB_ERR then AIndex := 0
    else if AIndex > MaxIndex then AIndex := MaxIndex;
    Selected[AIndex] := True;
  end;
  SetButtons;
end;

procedure TIndexFieldsEditDlg.UpdateSrcList;
var
  AFieldName: string;
  AIndex: Integer;
begin
  for AFieldName in DstList.Items do
  begin
    AIndex := SrcList.Items.IndexOf(AFieldName);
    if AIndex <> -1 then
      SrcList.Items.Delete(AIndex);
  end;
  SetButtons;
end;

procedure TIndexFieldsEditDlg.UpFldBtnClick(Sender: TObject);
var
  AIndex: Integer;
begin
  AIndex := GetFirstSelection(DstList);
  if (AIndex > 0) and (AIndex < DstList.Items.Count) then
  begin
    DstList.Items.Exchange(AIndex, AIndex - 1);
    SetItem(DstList, AIndex - 1);
  end;
end;

end.
