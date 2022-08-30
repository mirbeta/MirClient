{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPivotGrid                                         }
{                                                                    }
{           Copyright (c) 2005-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPIVOTGRID AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit cxPivotGridStyleSheetsPreview;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Classes, Controls, SysUtils, cxStyles,
  cxStyleSheetEditor, cxCustomData, cxDataUtils, cxDataStorage,
  cxCustomPivotGrid, cxPivotGrid;

type
  { TcxPivotGridStyleSheetEditorPreview }

  TcxPivotGridStyleSheetEditorPreview = class(TcxStyleSheetEditorPreview)
  private
    FPivotGrid: TcxPivotGrid;
  protected
    procedure CreatePreviewData; virtual;

    property PivotGrid: TcxPivotGrid read FPivotGrid;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetStyleSheetClass: TcxCustomStyleSheetClass; override;
    procedure SetStyleSheet(AStyleSheet: TcxCustomStyleSheet); override;
    function Control: TWinControl; override;
  end;

implementation

constructor TcxPivotGridStyleSheetEditorPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPivotGrid := TcxPivotGrid.Create(AOwner);
  CreatePreviewData;
  FPivotGrid.OptionsPrefilter.Visible := pfvAlways;
end;

destructor TcxPivotGridStyleSheetEditorPreview.Destroy;
begin
  FPivotGrid.Free;
  inherited Destroy;
end;

class function TcxPivotGridStyleSheetEditorPreview.GetStyleSheetClass: TcxCustomStyleSheetClass;
begin
  Result := TcxPivotGridStyleSheet;
end;

procedure TcxPivotGridStyleSheetEditorPreview.SetStyleSheet(AStyleSheet: TcxCustomStyleSheet);
begin
  PivotGrid.Styles.StyleSheet := AStyleSheet;
end;

function TcxPivotGridStyleSheetEditorPreview.Control: TWinControl;
begin
  Result := PivotGrid;
end;

procedure TcxPivotGridStyleSheetEditorPreview.CreatePreviewData;

  function AddColumn(const ACaption: string; AArea: TcxPivotGridFieldArea;
    AValueTypeClass: TcxValueTypeClass): TcxPivotGridField;
  begin
    Result := PivotGrid.CreateField;
    Result.Area := AArea;
    Result.Caption := ACaption;
    Result.DataBinding.ValueTypeClass := AValueTypeClass;
    Result.Visible := True;
  end;

  procedure AddValues(AValues: array of Variant);
  var
    I: Integer;
  begin
    with PivotGrid do
    begin
      for I := 0 to 2 do
      begin
        DataController.Append;
        try
          DataController.SetEditValue(0, AValues[0], evsValue);
          DataController.SetEditValue(1, AValues[I + 1], evsValue);
          DataController.SetEditValue(2, I * 4, evsValue);
        finally
          DataController.Post;
        end;
      end
    end;
  end;

begin
  PivotGrid.BeginUpdate;
  try
    AddColumn('Category Name', faRow, TcxStringValueType);
    AddColumn('Product Sales', faData, TcxCurrencyValueType);
    AddColumn('Shipped Quarter', faColumn,
      TcxDateTimeValueType).GroupInterval := giDateQuarter;
    AddValues(['Beverages', $109,753, $75,170, $30,943]);
    AddValues(['Condiments', $26,622, $35,754, $15,334]);
    AddValues(['Confections', $50,550, $54,833, $28,550]);
    AddValues(['Dairy Products', $65,432, $72,473, $39,973]);
    AddValues(['Grains/Cereals', $33,509, $22,249, $18,106]);
    AddValues(['Meat/Poultry', $56,060, $49,616, $17,664]);
    AddValues(['Produce', $22,277, $37,449, $15,842]);
    AddValues(['Seafood', $37,385, $34,434, $23,439]);
  finally
    PivotGrid.EndUpdate;
  end;
end;

initialization
  RegisterStyleSheetEditorPreview(TcxPivotGridStyleSheetEditorPreview);

finalization
  UnregisterStyleSheetEditorPreview(TcxPivotGridStyleSheetEditorPreview);

end.
