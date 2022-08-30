{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.Api.Images;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, Graphics,
  dxCore, dxCoreClasses, cxGeometry, dxGDIPlusClasses,
  dxRichEdit.NativeApi,
  dxRichEdit.Api.NativeDocument,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage;

type
  TdxNativeDocumentImageCollection = class;

  { TdxNativeDocumentImage }

  TdxNativeDocumentImage = class(TInterfacedObject, IdxRichEditDocumentImage)
  strict private
    FDocument: TdxNativeSubDocument;
    FRange: IdxRichEditDocumentRange;
    function GetImage: TdxOfficeImage;
    function GetInlinePictureRun: IdxPictureContainerRun;
    function GetLockAspectRatio: Boolean;
    function GetOriginalSize: TdxSizeF;
    function GetRange: IdxRichEditDocumentRange;
    function GetRectangleScalableObject: IdxRectangularScalableObject;
    function GetScaleX: Single;
    function GetScaleY: Single;
    function GetSize: TdxSizeF;
    function GetUri: string;
    procedure SetLockAspectRatio(const Value: Boolean);
    procedure SetScaleX(const Value: Single);
    procedure SetScaleY(const Value: Single);
    procedure SetSize(const Value: TdxSizeF);
    procedure SetUri(const Value: string);
  public
    constructor Create(ADocument: TdxNativeSubDocument; ARunIndex: TdxRunIndex);
    class function TryCreate(ADocument: TdxNativeSubDocument; ARun: TdxTextRunBase; ARunIndex: TdxRunIndex): TdxNativeDocumentImage; static;
    class function CanCreateImage(ADocument: TdxNativeSubDocument; ARun: TdxTextRunBase): Boolean; static;
    class function CreateUnsafe(ADocument: TdxNativeSubDocument; ARunIndex: TdxRunIndex): TdxNativeDocumentImage; static;
    class function CreateImage(AImage: TGraphic): TdxOfficeImage; static;

    property Image: TdxOfficeImage read GetImage;
    property InlinePictureRun: IdxPictureContainerRun read GetInlinePictureRun;
    property LockAspectRatio: Boolean read GetLockAspectRatio write SetLockAspectRatio;
    property OriginalSize: TdxSizeF read GetOriginalSize;
    property Range: IdxRichEditDocumentRange read FRange;
    property RectangleScalableObject: IdxRectangularScalableObject read GetRectangleScalableObject;
    property ScaleX: Single read GetScaleX write SetScaleX;
    property ScaleY: Single read GetScaleY write SetScaleY;
    property Size: TdxSizeF read GetSize write SetSize;
    property Uri: string read GetUri write SetUri;
  end;

  { TdxNativeDocumentImageCollection }

  TdxNativeDocumentImageCollection = class(TInterfacedObject,
    IdxRichEditDocumentImageCollection,
    IdxRichEditReadOnlyDocumentImageCollection)
  strict private
    type
      TProcessImageProc = reference to function(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex): Boolean;
  strict private
    FDocument: TdxNativeSubDocument;
    FRange: IdxRichEditDocumentRange;
    function CanProcessImage(const AImage: IdxRichEditDocumentImage): Boolean;
    function GetPieceTable: TdxPieceTable;
    function GetItem(Index: Integer): IdxRichEditDocumentImage;
    function GetCount: Integer;
    procedure ProcessImages(const Action: TProcessImageProc);
  public
    constructor Create(ADocument: TdxNativeSubDocument);

    function Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyDocumentImageCollection;

    function Append(AImage: TGraphic): IdxRichEditDocumentImage;
    function Insert(const APos: IdxRichEditDocumentPosition; AImage: TGraphic): IdxRichEditDocumentImage;

    property PieceTable: TdxPieceTable read GetPieceTable;
    property Self[Index: Integer]: IdxRichEditDocumentImage read GetItem; default;
    property Count: Integer read GetCount;
  end;

implementation

uses
  Math,
  dxTypeHelpers,
  dxRichEdit.DocumentModel.FloatingObjectRange;

{ TdxNativeDocumentImage }

constructor TdxNativeDocumentImage.Create(ADocument: TdxNativeSubDocument; ARunIndex: TdxRunIndex);
var
  AStart, AEnd: TdxDocumentModelPosition;
begin
  inherited Create;
  FDocument := ADocument;
  AStart := TdxDocumentModelPosition.FromRunStart(ADocument.PieceTable, ARunIndex);
  AEnd := AStart;
  AEnd.LogPosition := AStart.LogPosition + 1;
  FRange := TdxNativeDocumentRange.Create(ADocument, AStart, AEnd);
end;

class function TdxNativeDocumentImage.TryCreate(ADocument: TdxNativeSubDocument;
  ARun: TdxTextRunBase; ARunIndex: TdxRunIndex): TdxNativeDocumentImage;
var
  AAnchorRun: TdxFloatingObjectAnchorRun;
  AContent: TdxPictureFloatingObjectContent;
begin
  if ARun is TdxInlinePictureRun then
    Result := TdxNativeDocumentImage.Create(ADocument, ARunIndex)
  else
  begin
    Result := nil;
    AAnchorRun := Safe<TdxFloatingObjectAnchorRun>.Cast(ARun);
    if AAnchorRun <> nil then
    begin
      AContent := Safe<TdxPictureFloatingObjectContent>.Cast(AAnchorRun.Content);
      if AContent <> nil then
        Result := TdxNativeDocumentImage.Create(ADocument, ARunIndex);
    end;
  end;
end;

class function TdxNativeDocumentImage.CanCreateImage(ADocument: TdxNativeSubDocument;
  ARun: TdxTextRunBase): Boolean;
var
  AAnchorRun: TdxFloatingObjectAnchorRun;
  AContent: TdxPictureFloatingObjectContent;
begin
  Result := ARun is TdxInlinePictureRun;
  if not Result then
  begin
    AAnchorRun := Safe<TdxFloatingObjectAnchorRun>.Cast(ARun);
    if AAnchorRun <> nil then
    begin
      AContent := Safe<TdxPictureFloatingObjectContent>.Cast(AAnchorRun.Content);
      Result := AContent <> nil;
    end;
  end;
end;

class function TdxNativeDocumentImage.CreateUnsafe(ADocument: TdxNativeSubDocument;
  ARunIndex: TdxRunIndex): TdxNativeDocumentImage;
begin
  Result := TdxNativeDocumentImage.Create(ADocument, ARunIndex);
end;

class function TdxNativeDocumentImage.CreateImage(AImage: TGraphic): TdxOfficeImage;
var
  AStream: TStream;
begin
  if AImage = nil then
    Exit(nil);
  AStream := TMemoryStream.Create;
  try
    AImage.SaveToStream(AStream);
    AStream.Position := 0;
    Result := TdxOfficeImage.CreateImage(AStream);
  finally
    AStream.Free;
  end;
end;

function TdxNativeDocumentImage.GetInlinePictureRun: IdxPictureContainerRun;
begin
  Result := FDocument.PieceTable.Runs[TdxNativeDocumentPosition(FRange.Start).Position.RunIndex] as IdxPictureContainerRun;
end;

function TdxNativeDocumentImage.GetLockAspectRatio: Boolean;
begin
  Result := InlinePictureRun.LockAspectRatio;
end;

function TdxNativeDocumentImage.GetRectangleScalableObject: IdxRectangularScalableObject;
begin
  Result := FDocument.PieceTable.Runs[TdxNativeDocumentPosition(FRange.Start).Position.RunIndex] as IdxRectangularScalableObject;
end;

function TdxNativeDocumentImage.GetScaleX: Single;
begin
  Result := RectangleScalableObject.ScaleX / 100.0;
end;

procedure TdxNativeDocumentImage.SetLockAspectRatio(const Value: Boolean);
begin
  InlinePictureRun.LockAspectRatio := Value;
end;

procedure TdxNativeDocumentImage.SetScaleX(const Value: Single);
begin
  RectangleScalableObject.ScaleX := Value * 100.0;
end;

function TdxNativeDocumentImage.GetScaleY: Single;
begin
  Result := RectangleScalableObject.ScaleY / 100.0;
end;

procedure TdxNativeDocumentImage.SetScaleY(const Value: Single);
begin
  RectangleScalableObject.ScaleY := Value * 100.0;
end;

function TdxNativeDocumentImage.GetOriginalSize: TdxSizeF;
var
  AOriginalSize: TSize;
begin
  AOriginalSize := RectangleScalableObject.OriginalSize;
  Result := TdxSizeF.Create(FDocument.ModelUnitsToUnits(AOriginalSize.cx), FDocument.ModelUnitsToUnits(AOriginalSize.cy));
end;

function TdxNativeDocumentImage.GetRange: IdxRichEditDocumentRange;
begin
  Result := FRange;
end;

function TdxNativeDocumentImage.GetSize: TdxSizeF;
var
  AActualSize: TdxSizeF;
begin
  AActualSize := InlinePictureRun.ActualSizeF;
  Result := TdxSizeF.Create(FDocument.ModelUnitsToUnitsF(AActualSize.cx), FDocument.ModelUnitsToUnitsF(AActualSize.cy));
end;

procedure TdxNativeDocumentImage.SetSize(const Value: TdxSizeF);
var
  AWidth, AHeight: Integer;
begin
  AWidth := Max(1, FDocument.UnitsToModelUnits(Value.cx));
  AHeight := Max(1, FDocument.UnitsToModelUnits(Value.cy));
  InlinePictureRun.ActualSize := TSize.Create(AWidth, AHeight);
end;

function TdxNativeDocumentImage.GetImage: TdxOfficeImage;
begin
  Result := InlinePictureRun.PictureContent.Image.Image;
  Result.EnsureLoadComplete;
end;

function TdxNativeDocumentImage.GetUri: string;
begin
  Result := Image.Uri;
end;

procedure TdxNativeDocumentImage.SetUri(const Value: string);
begin
  Image.Uri := Value;
end;

{ TdxNativeDocumentImageCollection }

function TdxNativeDocumentImageCollection.CanProcessImage(
  const AImage: IdxRichEditDocumentImage): Boolean;
begin
  if AImage = nil then
    Exit(False);
  Result := (FRange = nil) or FRange.Contains(AImage.Range.Start);
end;

constructor TdxNativeDocumentImageCollection.Create(
  ADocument: TdxNativeSubDocument);
begin
  inherited Create;
  FDocument := ADocument;
end;

function TdxNativeDocumentImageCollection.Append(
  AImage: TGraphic): IdxRichEditDocumentImage;
begin
  Result := Insert(FDocument.EndPosition, AImage);
end;

function TdxNativeDocumentImageCollection.Get(
  const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyDocumentImageCollection;
begin
  FDocument.CheckValid;
  FDocument.CheckDocumentRange(ARange);

  Result := TdxNativeDocumentImageCollection.Create(FDocument);
  TdxNativeDocumentImageCollection(Result).FRange := ARange;
end;

function TdxNativeDocumentImageCollection.GetCount: Integer;
var
  AResult: Integer;
begin
  AResult := 0;
  ProcessImages(function(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex): Boolean
    var
      AImage: IdxRichEditDocumentImage;
    begin
      AImage := TdxNativeDocumentImage.TryCreate(FDocument, ARun, ARunIndex);
      if AImage = nil then
        Exit(False);
      if CanProcessImage(AImage) then
        Inc(AResult);
      Result := True;
    end);
  Result := AResult;
end;

function TdxNativeDocumentImageCollection.GetItem(
  Index: Integer): IdxRichEditDocumentImage;
var
  I: Integer;
  AResult: IdxRichEditDocumentImage;
begin
  I := 0;
  AResult := nil;
  ProcessImages(function(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex): Boolean
    var
      AImage: IdxRichEditDocumentImage;
    begin
      Result := False;
      AImage := TdxNativeDocumentImage.TryCreate(FDocument, ARun, ARunIndex);
      if AImage = nil then
        Exit;
      if CanProcessImage(AImage) then
      begin
        if I = Index then
          AResult := AImage
        else
          Inc(I);
        Result := AResult = nil;
      end;
    end);
  Result := AResult;
end;

function TdxNativeDocumentImageCollection.GetPieceTable: TdxPieceTable;
begin
  Result := FDocument.PieceTable;
end;

function TdxNativeDocumentImageCollection.Insert(
  const APos: IdxRichEditDocumentPosition;
  AImage: TGraphic): IdxRichEditDocumentImage;
var
  AOfficeImage: TdxOfficeImage;
  ARun: TdxInlinePictureRun;
  ARunInfo: TdxRunInfo;
  ALogPosition: TdxDocumentLogPosition;
begin
  FDocument.CheckValid;
  FDocument.CheckDocumentPosition(APos);

  if AImage = nil then
    Exit(nil);

  AOfficeImage := TdxNativeDocumentImage.CreateImage(AImage);
  ARunInfo := TdxRunInfo.Create(PieceTable);
  try
    ALogPosition := FDocument.NormalizeLogPosition(APos.LogPosition);
    ARun := TdxInlinePictureRun(PieceTable.InsertInlinePicture(ALogPosition, AOfficeImage));
    try
      if ARun = nil then
        Exit(nil);
      PieceTable.CalculateRunInfoStart(ALogPosition, ARunInfo);
      Result := TdxNativeDocumentImage.CreateUnsafe(FDocument, ARunInfo.Start.RunIndex);
    finally
      if ARun.Image.Image <> AOfficeImage then
        AOfficeImage.Free;
    end;
  finally
    ARunInfo.Free;
  end;
end;

procedure TdxNativeDocumentImageCollection.ProcessImages(
  const Action: TProcessImageProc);
var
  ARuns: TdxTextRunCollection;
  ARun: TdxTextRunBase;
  ACount, I: Integer;
begin
  ARuns := PieceTable.Runs;
  ACount := ARuns.Count;
  for I := 0 to ACount - 1 do
  begin
    ARun := ARuns[I];
    if TdxNativeDocumentImage.CanCreateImage(FDocument, ARun) then
      if not Action(ARun, I) then
        Break;
  end;
end;

end.
