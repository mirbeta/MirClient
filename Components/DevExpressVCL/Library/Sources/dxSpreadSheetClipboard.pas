{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetClipboard;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Graphics, Generics.Defaults, Generics.Collections, SysUtils, Clipbrd, cxClasses, cxGraphics,
  dxCore, cxVariants, dxHashUtils, dxSpreadSheetCore, dxSpreadSheetClasses, dxSpreadSheetTypes, dxSpreadSheetStrs,
  dxSpreadSheetGraphics, dxGDIPlusClasses, dxSpreadSheetHyperlinks, dxCoreClasses;

type

  { IdxSpreadSheetClipboardData }

  IdxSpreadSheetClipboardData = interface
  ['{3A8F2ACB-860C-4403-A1A1-F86DAD4C355D}']
    function GetMode: TdxSpreadSheetClipboardCopyMode;
    function GetViewID: string;
    procedure Paste(AView: TdxSpreadSheetTableView;
      const ADestPoint: TPoint; AOptions: TdxSpreadSheetClipboardPasteOptions);

    property Mode: TdxSpreadSheetClipboardCopyMode read GetMode;
    property ViewID: string read GetViewID;
  end;

  { TdxSpreadSheetCustomClipboardData }

  TdxSpreadSheetCustomClipboardData = class abstract(TInterfacedObject, IdxSpreadSheetClipboardData)
  protected
    FMode: TdxSpreadSheetClipboardCopyMode;
    FViewID: string;
  public
    // IdxSpreadSheetClipboardData
    function GetMode: TdxSpreadSheetClipboardCopyMode;
    function GetViewID: string;
    procedure Paste(AView: TdxSpreadSheetTableView; const ADestPoint: TPoint;
      AOptions: TdxSpreadSheetClipboardPasteOptions); virtual; abstract;
  end;

  { TdxSpreadSheetCustomClipboardFormat }

  TdxSpreadSheetCustomClipboardFormatClass = class of TdxSpreadSheetCustomClipboardFormat;
  TdxSpreadSheetCustomClipboardFormat = class abstract
  public
    class function Build(const AArea: TRect; AContainer: TdxSpreadSheetContainer;
      AMode: TdxSpreadSheetClipboardCopyMode; AView: TdxSpreadSheetTableView): IdxSpreadSheetClipboardData; virtual;
    class function CanLoadFromClipboard: Boolean; virtual;
    class function GetDescription: string; virtual;
    class function GetFormatID: Word; virtual;
    class function IsPasteOptionsSupported: Boolean; virtual;

    class function LoadFromClipboard: IdxSpreadSheetClipboardData; virtual;
    class function LoadFromStream(AStream: TStream): IdxSpreadSheetClipboardData; virtual;
    class procedure SaveToClipboard(AData: IdxSpreadSheetClipboardData); virtual;
    class procedure SaveToStream(AStream: TStream; AData: IdxSpreadSheetClipboardData); virtual;
  end;

  TdxSpreadSheetClipboardFormatEnumProc = reference to procedure (AFormat: TdxSpreadSheetCustomClipboardFormatClass);

  { TdxSpreadSheetTableViewPasteSelection }

  TdxSpreadSheetTableViewPasteSelection = class(TdxRectList)
  strict private
    FFocusedColumn: Integer;
    FFocusedRow: Integer;
  public
    procedure Restore(AView: TdxSpreadSheetTableView);
    procedure Store(AView: TdxSpreadSheetTableView; const AMinSize: TSize);
  end;

  { TdxSpreadSheetTableViewClipboardHelper }

  TdxSpreadSheetTableViewClipboardHelper = class
  protected
    class procedure CopyDataToClipboard(AView: TdxSpreadSheetTableView; AMode: TdxSpreadSheetClipboardCopyMode);
    class function IsPasteSpecial(AOptions: TdxSpreadSheetClipboardPasteOptions): Boolean;
    class function PreparePastePoints(ASelection: TdxSpreadSheetTableViewPasteSelection;
      const AClipboardArea: TRect; AClipboardCopyMode: TdxSpreadSheetClipboardCopyMode): TList<TPoint>;
    class function ReadDataFromClipboard(AOptions: TdxSpreadSheetClipboardPasteOptions;
      out AData: IdxSpreadSheetClipboardData): Boolean;
  public
    class procedure EnumFormats(AProc: TdxSpreadSheetClipboardFormatEnumProc); virtual;
    class procedure EnumFormatsForPaste(AProc: TdxSpreadSheetClipboardFormatEnumProc; AIsPasteSpecial: Boolean);

    class procedure Copy(AView: TdxSpreadSheetTableView);
    class procedure Cut(AView: TdxSpreadSheetTableView);

    class function CanPaste(AOptions: TdxSpreadSheetClipboardPasteOptions): Boolean;
    class procedure Paste(AView: TdxSpreadSheetTableView; const AClipboardArea: TRect;
      AClipboardCopyMode: TdxSpreadSheetClipboardCopyMode; const AViewGUID: string;
      AOptions: TdxSpreadSheetClipboardPasteOptions);
    class procedure PasteData(AView: TdxSpreadSheetTableView; AData: IdxSpreadSheetClipboardData;
      const AClipboardArea: TRect; AClipboardCopyMode: TdxSpreadSheetClipboardCopyMode; const AViewGUID: string;
      AOptions: TdxSpreadSheetClipboardPasteOptions);

    class procedure GetDataInfoFromClipboard(var AViewGUID: string;
      var ACopyMode: TdxSpreadSheetClipboardCopyMode; var AArea: TRect);
  end;

implementation

uses
  Math, Dialogs, Controls, Variants, StrUtils, dxSpreadSheetUtils, cxGeometry, dxSpreadSheetClipboardFormats,
  dxSpreadSheetCoreHelpers, dxSpreadSheetCoreStrs;

type
  TdxCustomSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);

{ TdxSpreadSheetCustomClipboardData }

function TdxSpreadSheetCustomClipboardData.GetMode: TdxSpreadSheetClipboardCopyMode;
begin
  Result := FMode;
end;

function TdxSpreadSheetCustomClipboardData.GetViewID: string;
begin
  Result := FViewID;
end;

{ TdxSpreadSheetCustomClipboardFormat }

class function TdxSpreadSheetCustomClipboardFormat.Build(const AArea: TRect; AContainer: TdxSpreadSheetContainer;
  AMode: TdxSpreadSheetClipboardCopyMode; AView: TdxSpreadSheetTableView): IdxSpreadSheetClipboardData;
begin
  Result := nil;
end;

class function TdxSpreadSheetCustomClipboardFormat.CanLoadFromClipboard: Boolean;
begin
  Result := Clipboard.HasFormat(GetFormatID);
end;

class function TdxSpreadSheetCustomClipboardFormat.GetDescription: string;
var
  ABuffer: array[0..64] of WideChar;
begin
  GetClipboardFormatName(GetFormatID, @ABuffer[0], Length(ABuffer));
  Result := ABuffer;
end;

class function TdxSpreadSheetCustomClipboardFormat.GetFormatID: Word;
begin
  Result := 0;
end;

class function TdxSpreadSheetCustomClipboardFormat.IsPasteOptionsSupported: Boolean;
begin
  Result := False;
end;

class function TdxSpreadSheetCustomClipboardFormat.LoadFromClipboard: IdxSpreadSheetClipboardData;
var
  AHandle: THandle;
  AMemory: Pointer;
  AMemoryStream: TMemoryStream;
begin
  Result := nil;
  if CanLoadFromClipboard then
  begin
    AHandle := Clipboard.GetAsHandle(GetFormatID);
    if AHandle <> 0 then
    begin
      AMemoryStream := TMemoryStream.Create;
      try
        AMemory := GlobalLock(AHandle);
        try
          AMemoryStream.Size := GlobalSize(AHandle);
          Move(AMemory^, AMemoryStream.Memory^, AMemoryStream.Size);
        finally
          GlobalUnlock(AHandle);
        end;
        Result := LoadFromStream(AMemoryStream);
      finally
        AMemoryStream.Free;
      end;
    end;
  end;
end;

class function TdxSpreadSheetCustomClipboardFormat.LoadFromStream(AStream: TStream): IdxSpreadSheetClipboardData;
begin
  Result := nil; // for CBuilder
end;

class procedure TdxSpreadSheetCustomClipboardFormat.SaveToClipboard(AData: IdxSpreadSheetClipboardData);
var
  AHandle: THandle;
  AMemory: Pointer;
  AMemoryStream: TMemoryStream;
begin
  AMemoryStream := TMemoryStream.Create;
  try
    SaveToStream(AMemoryStream, AData);
    if AMemoryStream.Size > 0 then
    begin
      AHandle := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, AMemoryStream.Size);
      AMemory := GlobalLock(AHandle);
      try
        Move(AMemoryStream.Memory^, AMemory^, AMemoryStream.Size);
      finally
        GlobalUnlock(AHandle);
      end;
      Clipboard.SetAsHandle(GetFormatID, AHandle);
    end;
  finally
    AMemoryStream.Free;
  end;
end;

class procedure TdxSpreadSheetCustomClipboardFormat.SaveToStream(AStream: TStream; AData: IdxSpreadSheetClipboardData);
begin
  // for CBuilder
end;

{ TdxSpreadSheetTableViewPasteSelection }

procedure TdxSpreadSheetTableViewPasteSelection.Restore(AView: TdxSpreadSheetTableView);
const
  AShift: array[Boolean] of TShiftState = ([], [ssCtrl]);
var
  AArea: TRect;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    AArea := Items[I];
    if I < Count - 1 then
      AView.Selection.Add(AArea, AShift[I > 0])
    else
      AView.Selection.Add(AArea, AShift[I > 0], FFocusedRow, FFocusedColumn);
  end;
  Clear;
end;

procedure TdxSpreadSheetTableViewPasteSelection.Store(AView: TdxSpreadSheetTableView; const AMinSize: TSize);
var
  I: Integer;
  AArea: TRect;
begin
  FFocusedRow := AView.Selection.FocusedRow;
  FFocusedColumn := AView.Selection.FocusedColumn;
  Clear;
  for I := 0 to AView.Selection.Count - 1 do
  begin
    AArea := AView.Selection[I].Rect;
    AArea.Right := Max(AArea.Right, AArea.Left + AMinSize.cx - 1);
    AArea.Bottom := Max(AArea.Bottom, AArea.Top + AMinSize.cy - 1);
    Add(AArea);
  end;
end;

{ TdxSpreadSheetTableViewClipboardHelper }

class procedure TdxSpreadSheetTableViewClipboardHelper.EnumFormats(AProc: TdxSpreadSheetClipboardFormatEnumProc);
begin
  AProc(TdxSpreadSheetBinaryClipboardFormat);
  AProc(TdxSpreadSheetXMLSSClipboardFormat);
  AProc(TdxSpreadSheetTextClipboardFormat);
  AProc(TdxSpreadSheetImageClipboardFormat);
  AProc(TdxSpreadSheetHTMLClipboardFormat);
end;

class procedure TdxSpreadSheetTableViewClipboardHelper.EnumFormatsForPaste(
  AProc: TdxSpreadSheetClipboardFormatEnumProc; AIsPasteSpecial: Boolean);
begin
  EnumFormats(
    procedure (AFormat: TdxSpreadSheetCustomClipboardFormatClass)
    begin
      if not AIsPasteSpecial or AFormat.IsPasteOptionsSupported then
        AProc(AFormat);
    end);
end;

class procedure TdxSpreadSheetTableViewClipboardHelper.Copy(AView: TdxSpreadSheetTableView);
begin
  CopyDataToClipboard(AView, ccmCopy);
end;

class procedure TdxSpreadSheetTableViewClipboardHelper.Cut(AView: TdxSpreadSheetTableView);
begin
  if AView.Selection.FocusedContainer <> nil then
  begin
    CopyDataToClipboard(AView, ccmCopy);
    AView.Selection.FocusedContainer.Free;
  end
  else
    CopyDataToClipboard(AView, ccmCut);
end;

class function TdxSpreadSheetTableViewClipboardHelper.CanPaste(AOptions: TdxSpreadSheetClipboardPasteOptions): Boolean;
var
  AResult: Boolean;
begin
  AResult := False;
  EnumFormatsForPaste(
    procedure (AFormat: TdxSpreadSheetCustomClipboardFormatClass)
    begin
      AResult := AResult or AFormat.CanLoadFromClipboard;
    end,
    IsPasteSpecial(AOptions));
  Result := AResult;
end;

class procedure TdxSpreadSheetTableViewClipboardHelper.Paste(AView: TdxSpreadSheetTableView;
  const AClipboardArea: TRect; AClipboardCopyMode: TdxSpreadSheetClipboardCopyMode;
  const AViewGUID: string; AOptions: TdxSpreadSheetClipboardPasteOptions);
var
  AData: IdxSpreadSheetClipboardData;
begin
  if ReadDataFromClipboard(AOptions, AData) then
    PasteData(AView, AData, AClipboardArea, AClipboardCopyMode, AViewGUID, AOptions);
end;

class procedure TdxSpreadSheetTableViewClipboardHelper.PasteData(
  AView: TdxSpreadSheetTableView; AData: IdxSpreadSheetClipboardData;
  const AClipboardArea: TRect; AClipboardCopyMode: TdxSpreadSheetClipboardCopyMode;
  const AViewGUID: string; AOptions: TdxSpreadSheetClipboardPasteOptions);
var
  AArea: TRect;
  AIsCannotChangePartOfArray: Boolean;
  ALocalArea: TRect;
  APastePoints: TList<TPoint>;
  ASelection: TdxSpreadSheetTableViewPasteSelection;
  ASourceView: TdxSpreadSheetTableViewAccess;
  I: Integer;
begin
  AIsCannotChangePartOfArray := False;
  AArea := cxRect(0, 0, dxSpreadSheetAreaWidth(AClipboardArea) - 1, dxSpreadSheetAreaHeight(AClipboardArea) - 1);
  ASelection := TdxSpreadSheetTableViewPasteSelection.Create;
  try
    ASelection.Store(AView, dxSpreadSheetAreaSize(AClipboardArea));
    APastePoints := PreparePastePoints(ASelection, AClipboardArea, AClipboardCopyMode);
    try
      AView.Selection.Clear;
      for I := 0 to APastePoints.Count - 1 do
      begin
        ALocalArea := cxRectOffset(AArea, APastePoints[I]);
        AView.Selection.Add(ALocalArea, []);
        AIsCannotChangePartOfArray := not TdxSpreadSheetTableViewAccess(AView).CanModifyDataInArrayFormulaArea(ALocalArea, cmmClear);
        if AIsCannotChangePartOfArray then
          Break
        else
          AData.Paste(AView, AView.Selection.Area.TopLeft, AOptions);
      end;
      if APastePoints.Count > 1 then
        ASelection.Restore(AView);
    finally
      APastePoints.Free;
    end;
  finally
    ASelection.Free;
  end;

  if not AIsCannotChangePartOfArray and (AClipboardCopyMode = ccmCut) then
  begin
    ASourceView := TdxSpreadSheetTableViewAccess(TdxCustomSpreadSheetAccess(AView.SpreadSheet).GetSheetByGUID(AViewGUID));
    if ASourceView <> nil then
    begin
      if ASourceView = AView then
        ASourceView.ClearCells(AClipboardArea, ALocalArea, dxSpreadSheetTableViewClearCellsOptionsAll)
      else
        ASourceView.ClearCells(AClipboardArea, dxSpreadSheetTableViewClearCellsOptionsAll);
    end;
    TdxCustomSpreadSheetAccess(AView.SpreadSheet).ClearClipboard;
  end;

  if AIsCannotChangePartOfArray then
    raise EdxSpreadSheetCannotChangePartOfArrayError.Create(cxGetResourceString(@sdxErrorCannotChangePartOfArray));
end;

class procedure TdxSpreadSheetTableViewClipboardHelper.GetDataInfoFromClipboard(
  var AViewGUID: string; var ACopyMode: TdxSpreadSheetClipboardCopyMode; var AArea: TRect);
var
  ANativeFormat: TdxSpreadSheetNativeClipboardFormatClass;
begin
  ANativeFormat := nil;
  EnumFormats(
    procedure (AFormat: TdxSpreadSheetCustomClipboardFormatClass)
    begin
      if ANativeFormat = nil then
      begin
        if AFormat.InheritsFrom(TdxSpreadSheetNativeClipboardFormat) and AFormat.CanLoadFromClipboard then
          ANativeFormat := TdxSpreadSheetNativeClipboardFormatClass(AFormat);
      end;
    end);

  if (ANativeFormat = nil) or not ANativeFormat.GetInfoFromClipboard(AViewGUID, ACopyMode, AArea) then
  begin
    AArea := cxInvalidRect;
    ACopyMode := ccmNone;
    AViewGUID := '';
  end;
end;

class procedure TdxSpreadSheetTableViewClipboardHelper.CopyDataToClipboard(
  AView: TdxSpreadSheetTableView; AMode: TdxSpreadSheetClipboardCopyMode);
begin
  Clipboard.Open;
  try
    Clipboard.Clear;
    EnumFormats(
      procedure (AFormat: TdxSpreadSheetCustomClipboardFormatClass)
      var
        AData: IdxSpreadSheetClipboardData;
      begin
        AData := AFormat.Build(AView.Selection.Area, AView.Selection.FocusedContainer, AMode, AView);
        if AData <> nil then
          AFormat.SaveToClipboard(AData);
      end);
  finally
    Clipboard.Close;
  end;
end;

class function TdxSpreadSheetTableViewClipboardHelper.IsPasteSpecial(
  AOptions: TdxSpreadSheetClipboardPasteOptions): Boolean;
begin
  Result := AOptions <> dxSpreadSheetDefaultPasteOptions;
end;

class function TdxSpreadSheetTableViewClipboardHelper.PreparePastePoints(
  ASelection: TdxSpreadSheetTableViewPasteSelection; const AClipboardArea: TRect;
  AClipboardCopyMode: TdxSpreadSheetClipboardCopyMode): TList<TPoint>;
var
  AArea, R: TRect;
  AInArea: Boolean;
  AIsCopyOneCell: Boolean;
  AWidth, AHeight: Integer;
  I: Integer;
begin
  Result := TList<TPoint>.Create;
  if cxRectIsEqual(cxInvalidRect, AClipboardArea) then
  begin
    Result.Add(ASelection.Last.TopLeft);
    Exit;
  end;

  AWidth := dxSpreadSheetAreaWidth(AClipboardArea);
  AHeight := dxSpreadSheetAreaHeight(AClipboardArea);
  AIsCopyOneCell := (AWidth = 1) and (AHeight = 1);
  for I := 0 to ASelection.Count - 1 do
  begin
    AArea := ASelection[I];
    R := cxRectSetSize(AArea, AWidth - 1, AHeight - 1);
    while dxSpreadSheetContains(AArea, R.Top, R.Left) do
    begin
      Result.Add(R.TopLeft);
      if AClipboardCopyMode = ccmCut then
        Break;

      AInArea := dxSpreadSheetContains(AArea, R.Bottom, R.Right);
      if (ASelection.Count = 1) or AIsCopyOneCell or AInArea then
      begin
        if not AInArea then
        begin
          while Result.Count > 1 do
            Result.Delete(Result.Count - 1);
          Break;
        end;
        R := cxRectOffset(R, AWidth, 0);
        if R.Left > AArea.Right then
          R := cxRectOffset(R, AArea.Left - R.Left, AHeight);
      end
      else
        Break;
    end;
  end;
end;

class function TdxSpreadSheetTableViewClipboardHelper.ReadDataFromClipboard(
  AOptions: TdxSpreadSheetClipboardPasteOptions; out AData: IdxSpreadSheetClipboardData): Boolean;
var
  ATempData: IdxSpreadSheetClipboardData;
begin
  Clipboard.Open;
  try
    ATempData := nil;
    EnumFormatsForPaste(
      procedure (AFormat: TdxSpreadSheetCustomClipboardFormatClass)
      begin
        if ATempData = nil then
          ATempData := AFormat.LoadFromClipboard;
      end,
      IsPasteSpecial(AOptions));

    Result := ATempData <> nil;
    if Result then
      AData := ATempData;
  finally
    Clipboard.Close;
  end;
end;

end.
