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

unit dxBarCodeUtils;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Graphics, SysUtils,
  dxCore, cxClasses, cxGeometry, cxGraphics;

{$REGION 'Offset Constants'}

const
  dxBarCodeEAN13LeftQuietZoneSize: Integer = 11;
  dxBarCodeEAN13RightQuietZoneSize: Integer = 7;
  dxBarCodeEAN8LeftQuietZoneSize: Integer = 7;
  dxBarCodeEAN8RightQuietZoneSize: Integer = 7;
  dxBarCodeUPCALeftQuietZoneSize: Integer = 9;
  dxBarCodeUPCARightQuietZoneSize: Integer = 9;
  dxBarCodeUPCELeftQuietZoneSize: Integer = 9;
  dxBarCodeUPCERightQuietZoneSize: Integer = 7;
  dxBarCode128LeftQuietZoneSize: Integer = 10;
  dxBarCode128RightQuietZoneSize: Integer = 10;
  dxBarCodeInterleaved2Of5LeftQuietZoneSize: Integer = 10;
  dxBarCodeInterleaved2Of5RightQuietZoneSize: Integer = 10;
  dxBarCode39LeftQuietZoneSize: Integer = 10;
  dxBarCode39RightQuietZoneSize: Integer = 10;
  dxBarCode93LeftQuietZoneSize: Integer = 10;
  dxBarCode93RightQuietZoneSize: Integer = 10;
  dxBarCodeMSILeftQuietZoneSize: Integer = 2;
  dxBarCodeMSIRightQuietZoneSize: Integer = 2;
  dxBarCode11LeftQuietZoneSize: Integer = 2;
  dxBarCode11RightQuietZoneSize: Integer = 2;
  dxBarCodeQRCodeLeftQuietZoneSize: Integer = 3;
  dxBarCodeQRCodeRightQuietZoneSize: Integer = 3;
  dxBarCodeQRCodeTopQuietZoneSize: Integer = 3;
  dxBarCodeQRCodeBottomQuietZoneSize: Integer = 3;

{$ENDREGION}

type
  TdxBit = (bit0, bit1);
  TdxBitArray = array of TdxBit;
  Tdx2DBitArray = array of TdxBitArray;
  TdxQRCodeBlocks = array[0..80] of array of Integer;
  TdxBarCodeErrorType = (bceNone, bceInvalidCharacters, bceInvalidTextFormat, bceBoundsTooSmall);
  TdxBarCodeFitMode = ifmNormal..ifmFit;

  TdxCustomBarCodeGeneratorClass = class of TdxCustomBarCodeGenerator;
  TdxCustomBarCodeSymbologyClass = class of TdxCustomBarCodeSymbology;

  TdxCustomBarCodeSymbology = class;
  TdxBarCodeEAN13Symbology = class;
  TdxBarCodeEAN8Symbology = class;
  TdxBarCodeUPCASymbology = class;
  TdxBarCodeUPCESymbology = class;
  TdxBarCode128Symbology = class;
  TdxBarCodeITFTypeSymbology = class;
  TdxBarCodeInterleaved2Of5Symbology = class;
  TdxBarCode39Symbology = class;
  TdxBarCode39ExtendedSymbology = class;
  TdxBarCode93Symbology = class;
  TdxBarCode93ExtendedSymbology = class;
  TdxBarCodeMSISymbology = class;
  TdxBarCode11Symbology = class;
  TdxBarCodeQRCodeSymbology = class;

  { TdxBarCodeCustomPainter }

  TdxBarCodeCustomPainter = class abstract
  public
    procedure FillRect(const ARect: TRect; AColor: TColor); virtual; abstract;
    procedure DrawText(const ARect: TRect; const AText: string; AFormat: Integer; AFont: TFont); virtual; abstract;
  end;

  { TdxBarCodePainter }

  TdxBarCodePainter = class(TdxBarCodeCustomPainter)
  strict private
    FCanvas: TcxCanvas;
  public
    procedure FillRect(const ARect: TRect; AColor: TColor); override;
    procedure DrawText(const ARect: TRect; const AText: string; AFormat: Integer; AFont: TFont); override;

    property Canvas: TcxCanvas read FCanvas write FCanvas;
  end;

  { TdxCustomBarCodeGenerator }

  TdxCustomBarCodeGenerator = class
  strict private
    function GetSymbology: TdxCustomBarCodeSymbology;
  private
    procedure AdjustFontSize(const AText: string; AAvailableWidth: Integer; AFont: TFont);
  protected
    FPainter: TdxBarCodeCustomPainter;
    FSymbology: TdxCustomBarCodeSymbology;

    function CalculateSize(const AText: string; ASymbology: TdxCustomBarCodeSymbology; const AFont: TFont; out AWidth: Integer; out AHeight: Integer): TdxBarCodeErrorType; virtual;
    function Paint(APainter: TdxBarCodeCustomPainter; const ABounds: TRect; const AText: string; ASymbology: TdxCustomBarCodeSymbology; const AFont: TFont): TdxBarCodeErrorType; overload; virtual;

    procedure AdjustCodeText(var ACodeText: string); virtual;
    function CheckText(const AText: string): TdxBarCodeErrorType; virtual;
    function GetCodeText(const AText: string): string; virtual;
    function GetTextForDrawing(const ACodeText: string): string; virtual;

    function CalculateRect(const ACodeText: string; AFont: TFont; var ARect: TRect): TdxBarCodeErrorType; virtual;
    function DefaultBarHeight: Integer; virtual;
    function GetCodeWidth(const ACodeText: string): Integer; virtual;
    function GetInternalSize(const ACodeText: string; const AFont: TFont): TSize; virtual;
    function GetScaledFontHeight(const ABounds: TRect; AFont: TFont; const ACodeText: string): Integer; virtual;
    function GetTextHeight(const AText: string; AFont: TFont): Integer; virtual;
    function GetTextRect(const ABoundsRect, ACodeRect: TRect): TRect; virtual;
    function ScaleX(const AClientSize, ADefaultSize: TSize): Single; virtual;
    function ScaleY(const AClientSize, ADefaultSize: TSize): Single; virtual;

    procedure DrawBars(const ABoundsRect: TRect; const ACodeText: string; const ABarSize: TdxSizeF); virtual;
    procedure DrawText(const ACodeText: string; const ARect: TRect; const ABarSize: TdxSizeF; AFont: TFont); virtual;

    // Offsets
    function GetBottomOffset: Integer; virtual;
    function GetLeftOffset: Integer; virtual;
    function GetRightOffset: Integer; virtual;
    function GetTopOffset: Integer; virtual;

    property Symbology: TdxCustomBarCodeSymbology read GetSymbology;
  end;

  { TdxBarCodeOneDimensionalGenerator }

  TdxBarCodeOneDimensionalGenerator = class(TdxCustomBarCodeGenerator)
  strict private
    function GetSymbology: TdxCustomBarCodeSymbology;
  protected
    // TdxCustomBarCodeGenerator
    function CalculateRect(const ACodeText: string; AFont: TFont; var ARect: TRect): TdxBarCodeErrorType; override;
    function CalculateSize(const AText: string; ASymbology: TdxCustomBarCodeSymbology; const AFont: TFont; out AWidth: Integer; out AHeight: Integer): TdxBarCodeErrorType; override;
    function DefaultBarHeight: Integer; override;
    procedure DrawText(const ACodeText: string; const ARect: TRect; const ABarSize: TdxSizeF; AFont: TFont); override;

    function GetBinaryCode(const ACodeText: string): TdxBitArray; virtual;
    function GetBinaryCodeLength(const ACodeText: string): Integer; virtual;
    function GetFullSymbolicCode(const ACodeText: string): string; virtual;
    function GetFullSymbolicCodeLength(const ACodeText: string): Integer; virtual;
    function GetModuleForSymbolCount: Integer; virtual;
    function GetStartBinaryCode: string; virtual;
    function GetStopBinaryCode: string; virtual;

    procedure PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray; ANumberPos: Integer; var AModulePos: Integer); virtual;

    property Symbology: TdxCustomBarCodeSymbology read GetSymbology;
  end;

  { TdxBarCodeEANTypeGenerator }

  TdxBarCodeEANTypeGenerator = class(TdxBarCodeOneDimensionalGenerator)
  protected
    // TdxCustomBarCodeGenerator
    procedure DrawBars(const ABoundsRect: TRect; const ACodeText: string; const ABarSize: TdxSizeF); override;
    function GetCodeWidth(const ACodeText: string): Integer; override;

    function GetLongBarHeight(ABarWidth, ABarHeight: Single): Single; virtual;
    function GetLongLinePatternMask(const ACodeText: string): TdxBitArray; virtual;
  end;

  { TdxBarCodeEAN13Generator }

  TdxBarCodeEAN13Generator = class(TdxBarCodeEANTypeGenerator)
  strict private
    function GetSymbology: TdxBarCodeEAN13Symbology;
  protected
    // TdxCustomBarCode
    procedure AdjustCodeText(var ACodeText: string); override;
    function CheckText(const AText: string): TdxBarCodeErrorType; override;
    function DefaultBarHeight: Integer; override;
    procedure DrawText(const ACodeText: string; const ARect: TRect; const ABarSize: TdxSizeF; AFont: TFont); override;
    function GetScaledFontHeight(const ABounds: TRect; AFont: TFont; const ACodeText: string): Integer; override;
    function GetTextForDrawing(const ACodeText: string): string; override;

    // TdxOneDimensionalBarCode
    function GetBinaryCodeLength(const ACodeText: string): Integer; override;
    function GetFullSymbolicCode(const ACodeText: string): string; override;
    function GetLongLinePatternMask(const ACodeText: string): TdxBitArray; override;
    function GetLongBarHeight(ABarWidth, ABarHeight: Single): Single; override;
    function GetModuleForSymbolCount: Integer; override;
    function GetStartBinaryCode: string; override;
    function GetStopBinaryCode: string; override;
    function GetTextRect(const ABoundsRect, ACodeRect: TRect): TRect; override;
    procedure PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray; ANumberPos: Integer; var AModulePos: Integer); override;

    // Offsets
    function GetLeftOffset: Integer; override;
    function GetRightOffset: Integer; override;

    function GetTextHeight(const AText: string; AFont: TFont): Integer; override;

    function GetCenterCode: string;
    function GetChecksum(const ACodeText: string): string;

    property Symbology: TdxBarCodeEAN13Symbology read GetSymbology;
  end;

  { TdxBarCodeEAN8Generator }

  TdxBarCodeEAN8Generator = class(TdxBarCodeEANTypeGenerator)
  strict private
    function GetSymbology: TdxBarCodeEAN8Symbology;
  protected
    // TdxCustomBarCode
    procedure AdjustCodeText(var ACodeText: string); override;
    function CheckText(const AText: string): TdxBarCodeErrorType; override;
    function DefaultBarHeight: Integer; override;
    procedure DrawText(const ACodeText: string; const ARect: TRect; const ABarSize: TdxSizeF; AFont: TFont); override;
    function GetScaledFontHeight(const ABounds: TRect; AFont: TFont; const ACodeText: string): Integer; override;
    function GetTextForDrawing(const ACodeText: string): string; override;

    // TdxOneDimensionalBarCode
    function GetBinaryCodeLength(const ACodeText: string): Integer; override;
    function GetFullSymbolicCode(const ACodeText: string): string; override;
    function GetLongLinePatternMask(const ACodeText: string): TdxBitArray; override;
    function GetLongBarHeight(ABarWidth, ABarHeight: Single): Single; override;
    function GetModuleForSymbolCount: Integer; override;
    function GetStartBinaryCode: string; override;
    function GetStopBinaryCode: string; override;
    function GetTextRect(const ABoundsRect, ACodeRect: TRect): TRect; override;
    procedure PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray; ANumberPos: Integer; var AModulePos: Integer); override;

    // Offsets
    function GetLeftOffset: Integer; override;
    function GetRightOffset: Integer; override;

    function GetCenterCode: string;
    function GetChecksum(const ACodeText: string): string;
    function GetTextHeight(const AText: string; AFont: TFont): Integer; override;

    property Symbology: TdxBarCodeEAN8Symbology read GetSymbology;
  end;

  { TdxBarCodeUPCAGenerator }

  TdxBarCodeUPCAGenerator = class(TdxBarCodeEANTypeGenerator)
  strict private
    function GetSymbology: TdxBarCodeUPCASymbology;
  protected
    // TdxCustomBarCode
    procedure AdjustCodeText(var ACodeText: string); override;
    function CheckText(const AText: string): TdxBarCodeErrorType; override;
    function DefaultBarHeight: Integer; override;
    procedure DrawText(const ACodeText: string; const ARect: TRect; const ABarSize: TdxSizeF; AFont: TFont); override;
    function GetScaledFontHeight(const ABounds: TRect; AFont: TFont; const ACodeText: string): Integer; override;
    function GetTextForDrawing(const ACodeText: string): string; override;

    // TdxOneDimensionalBarCode
    function GetBinaryCodeLength(const ACodeText: string): Integer; override;
    function GetFullSymbolicCode(const ACodeText: string): string; override;
    function GetLongLinePatternMask(const ACodeText: string): TdxBitArray; override;
    function GetLongBarHeight(ABarWidth, ABarHeight: Single): Single; override;
    function GetModuleForSymbolCount: Integer; override;
    function GetStartBinaryCode: string; override;
    function GetStopBinaryCode: string; override;
    function GetTextRect(const ABoundsRect, ACodeRect: TRect): TRect; override;
    procedure PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray; ANumberPos: Integer; var AModulePos: Integer); override;

    // Offsets
    function GetLeftOffset: Integer; override;
    function GetRightOffset: Integer; override;

    function GetCenterCode: string;
    function GetChecksum(const ACodeText: string): string;
    function GetTextHeight(const AText: string; AFont: TFont): Integer; override;

    property Symbology: TdxBarCodeUPCASymbology read GetSymbology;
  end;

  { TdxBarCodeUPCEGenerator }

  TdxBarCodeUPCEGenerator = class(TdxBarCodeEANTypeGenerator)
  strict private
    function GetSymbology: TdxBarCodeUPCESymbology;
  protected
    // TdxCustomBarCode
    procedure AdjustCodeText(var ACodeText: string); override;
    function CheckText(const AText: string): TdxBarCodeErrorType; override;
    function DefaultBarHeight: Integer; override;
    procedure DrawText(const ACodeText: string; const ARect: TRect; const ABarSize: TdxSizeF; AFont: TFont); override;
    function GetScaledFontHeight(const ABounds: TRect; AFont: TFont; const ACodeText: string): Integer; override;
    function GetTextForDrawing(const ACodeText: string): string; override;

    // TdxOneDimensionalBarCode
    function GetBinaryCodeLength(const ACodeText: string): Integer; override;
    function GetFullSymbolicCode(const ACodeText: string): string; override;
    function GetLongLinePatternMask(const ACodeText: string): TdxBitArray; override;
    function GetLongBarHeight(ABarWidth, ABarHeight: Single): Single; override;
    function GetModuleForSymbolCount: Integer; override;
    function GetStartBinaryCode: string; override;
    function GetStopBinaryCode: string; override;
    function GetTextRect(const ABoundsRect, ACodeRect: TRect): TRect; override;
    procedure PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray; ANumberPos: Integer; var AModulePos: Integer); override;

    // Offsets
    function GetLeftOffset: Integer; override;
    function GetRightOffset: Integer; override;

    function GetChecksum(const ACodeText: string): string;
    function GetTextHeight(const AText: string; AFont: TFont): Integer; override;

    property Symbology: TdxBarCodeUPCESymbology read GetSymbology;
  end;

  TdxBarCode128CharacterSet = (csAuto, csA, csB, csC);

  { TdxBarCode128Generator }

  TdxBarCode128Generator = class(TdxBarCodeEANTypeGenerator)
  strict private
    function CheckCodeByCharset(const ACodeText: string; ACharset: TdxBarCode128CharacterSet): TdxBarCodeErrorType;
    function GetIndexBySymbol(ASymbol: Char): Integer;
    function GetSymbology: TdxBarCode128Symbology;
  protected
    // TdxOneDimensionalBarCode
    function GetFullSymbolicCode(const ACodeText: string): string; override;
    function GetInternalSize(const ACodeText: string; const AFont: TFont): TSize; override;
    function CheckText(const AText: string): TdxBarCodeErrorType; override;
    function GetModuleForSymbolCount: Integer; override;
    function GetStopBinaryCode: string; override;
    procedure PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray; ANumberPos: Integer; var AModulePos: Integer); override;

    // Offsets
    function GetLeftOffset: Integer; override;
    function GetRightOffset: Integer; override;

    function GetChecksum(const ACodeText: string): string;
    function IsEAN: Boolean; virtual;

    property Symbology: TdxBarCode128Symbology read GetSymbology;
  end;

  { TdxBarCodeITFTypeGenerator }

  TdxBarCodeITFTypeGenerator = class(TdxBarCodeOneDimensionalGenerator)
  strict private
    function GetSymbology: TdxBarCodeITFTypeSymbology;
  protected
    procedure DrawBars(const ABoundsRect: TRect; const ACodeText: string; const ABarSize: TdxSizeF); override;
    function GetCodeWidth(const ACodeText: string): Integer; override;
    function GetInternalSize(const ACodeText: string; const AFont: TFont): TSize; override;

    property Symbology: TdxBarCodeITFTypeSymbology read GetSymbology;
  end;

  { TdxBarCodeInterleaved2Of5Generator }

  TdxBarCodeInterleaved2Of5Generator = class(TdxBarCodeITFTypeGenerator)
  strict private
    function GetSymbology: TdxBarCodeInterleaved2Of5Symbology;
  protected
    // TdxCustomBarCode
    function CheckText(const AText: string): TdxBarCodeErrorType; override;
    function GetTextForDrawing(const ACodeText: string): string; override;

    // TdxOneDimensionalBarCode
    function GetFullSymbolicCode(const ACodeText: string): string; override;
    function GetModuleForSymbolCount: Integer; override;
    function GetStartBinaryCode: string; override;
    function GetStopBinaryCode: string; override;
    procedure PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray; ANumberPos: Integer; var AModulePos: Integer); override;

    // Offsets
    function GetLeftOffset: Integer; override;
    function GetRightOffset: Integer; override;

    function GetChecksum(const ACodeText: string): string;

    property Symbology: TdxBarCodeInterleaved2Of5Symbology read GetSymbology;
  end;

  { TdxBarCode39Generator }

  TdxBarCode39Generator = class(TdxBarCodeITFTypeGenerator)
  strict private
    function GetCharMapIndex(const AFullSymbolicCode: string; ACharIndex: Integer): Integer;
    function GetEdgeCode: string;
    function GetSymbology: TdxBarCode39Symbology;
  protected
    // TdxCustomBarCode
    function CheckText(const AText: string): TdxBarCodeErrorType; override;
    function GetTextForDrawing(const ACodeText: string): string; override;

    // TdxOneDimensionalBarCode
    function GetFullSymbolicCode(const ACodeText: string): string; override;
    function GetModuleForSymbolCount: Integer; override;
    function GetStartBinaryCode: string; override;
    function GetStopBinaryCode: string; override;
    procedure PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray; ANumberPos: Integer; var AModulePos: Integer); override;

    // Offsets
    function GetLeftOffset: Integer; override;
    function GetRightOffset: Integer; override;

    function GetChecksum(const ACodeText: string): string;

    property Symbology: TdxBarCode39Symbology read GetSymbology;
  end;

  { TdxBarCode39ExtendedGenerator }

  TdxBarCode39ExtendedGenerator = class(TdxBarCode39Generator)
  strict private
    function GetSymbology: TdxBarCode39ExtendedSymbology;
  protected
    // TdxCustomBarCode
    function CheckText(const AText: string): TdxBarCodeErrorType; override;
    function GetTextForDrawing(const ACodeText: string): string; override;

    // TdxOneDimensionalBarCode
    function GetFullSymbolicCode(const ACodeText: string): string; override;

    property Symbology: TdxBarCode39ExtendedSymbology read GetSymbology;
  end;

  { TdxBarCode93Generator }

  TdxBarCode93Generator = class(TdxBarCodeEANTypeGenerator)
  strict private
    function GetChecksum(const ACodeText: string; AWeighting: Integer): string;
    function GetSymbolNumber(ASymbol: Char): Integer;
    function GetSymbology: TdxBarCode93Symbology;
  protected
    // TdxCustomBarCode
    function CheckText(const AText: string): TdxBarCodeErrorType; override;

    // TdxOneDimensionalBarCode
    function GetFullSymbolicCode(const ACodeText: string): string; override;
    function GetInternalSize(const ACodeText: string; const AFont: TFont): TSize; override;
    function GetModuleForSymbolCount: Integer; override;
    function GetStartBinaryCode: string; override;
    function GetStopBinaryCode: string; override;

    // Offsets
    function GetLeftOffset: Integer; override;
    function GetRightOffset: Integer; override;

    procedure PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray; ANumberPos: Integer; var AModulePos: Integer); override;

    property Symbology: TdxBarCode93Symbology read GetSymbology;
  end;

  { TdxBarCode93ExtendedGenerator }

  TdxBarCode93ExtendedGenerator = class(TdxBarCode93Generator)
  strict private
    function GetSymbology: TdxBarCode93ExtendedSymbology;
  protected
    // TdxCustomBarCode
    function CheckText(const AText: string): TdxBarCodeErrorType; override;

    // TdxOneDimensionalBarCode
    function GetFullSymbolicCode(const ACodeText: string): string; override;

    property Symbology: TdxBarCode93ExtendedSymbology read GetSymbology;
  end;

  { TdxBarCodeMSIGenerator }

  TdxBarCodeMSIGenerator = class(TdxBarCodeEANTypeGenerator)
  strict private
    function GetSymbology: TdxBarCodeMSISymbology;
  protected
    // TdxCustomBarCode
    function CheckText(const AText: string): TdxBarCodeErrorType; override;
    function GetTextForDrawing(const ACodeText: string): string; override;

    // TdxOneDimensionalBarCode
    function GetFullSymbolicCode(const ACodeText: string): string; override;
    function GetInternalSize(const ACodeText: string; const AFont: TFont): TSize; override;
    function GetModuleForSymbolCount: Integer; override;
    function GetStartBinaryCode: string; override;
    function GetStopBinaryCode: string; override;
    procedure PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray; ANumberPos: Integer; var AModulePos: Integer); override;

    // Offsets
    function GetLeftOffset: Integer; override;
    function GetRightOffset: Integer; override;

    function GetChecksum(const ACodeText: string): string;

    property Symbology: TdxBarCodeMSISymbology read GetSymbology;
  end;

  { TdxBarCode11Generator }

  TdxBarCode11Generator = class(TdxBarCodeITFTypeGenerator)
  strict private
    function GetSymbology: TdxBarCode11Symbology;
  protected
    // TdxCustomBarCode
    function CheckText(const AText: string): TdxBarCodeErrorType; override;

    // TdxOneDimensionalBarCode
    function GetFullSymbolicCode(const ACodeText: string): string; override;
    function GetModuleForSymbolCount: Integer; override;
    function GetStartBinaryCode: string; override;
    function GetStopBinaryCode: string; override;
    procedure PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray; ANumberPos: Integer; var AModulePos: Integer); override;

    // Offsets
    function GetLeftOffset: Integer; override;
    function GetRightOffset: Integer; override;

    function GetChecksum(const ACodeText: string): string;
    function GetSymbolNumber(ASymbol: Char): Integer;

    property Symbology: TdxBarCode11Symbology read GetSymbology;
  end;

  { TdxBarCodeQRCodeGenerator }

  TdxQRCodeCompactionMode = (qrmNumeric, qrmAlphaNumeric, qrmByte);
  TdxQRCodeErrorCorrectionLevel = (eclL, eclM, eclQ, eclH);

  TdxBarCodeQRCodeGenerator = class(TdxCustomBarCodeGenerator)
  strict private
    function GetSymbology: TdxBarCodeQRCodeSymbology;

    procedure AddBinaryData(ABinaryCode: TdxBitArray; const ACodeText: string);
    procedure AddIndicatorCharCount(ABinaryCode: TdxBitArray; ACharCount: Integer);
    procedure AddModeIndicator(ABinaryCode: TdxBitArray);
    procedure AddNextValue(ABinaryCode: TdxBitArray; AValue, APosition, ASymbolCountInValue: Integer);
    procedure AddNullPattern(ABinaryCode: TdxBitArray; AStartWordPosition: Integer);
    procedure CalculateCodeWords(ABinaryCode: TdxBitArray; var ACodeWords: TdxQRCodeBlocks);
    procedure CalculateErrorCorrectionWords(ACodeWords: TdxQRCodeBlocks; var AErrorWords: TdxQRCodeBlocks);
    function CreateBinaryCodeFromText(const ACodeText: string): TdxBitArray;
    function CreateResultBinaryCode(ACodeWords, AErrorWords: TdxQRCodeBlocks): TdxBitArray;
    function GetBlockCount: Integer;
    function GetCharCountIndicatorLength(AVersion: Integer): Integer;
    function GetDataWordCount: Integer;
    function GetErrorCorrectionWordInBlockCount: Integer;
    procedure PopulateBinaryCodeForAlphaNumericType(ABinaryCode: TdxBitArray; const ACodeText: string; var APosition: Integer);
    procedure PopulateBinaryCodeForByteType(ABinaryCode: TdxBitArray; const ACodeText: string; var APosition: Integer);
    procedure PopulateBinaryCodeForNumericType(ABinaryCode: TdxBitArray; const ACodeText: string; var APosition: Integer);

    procedure AddAlignmentPatternsToMatrix(ARegularMatrix, ARegularMatrixMask: Tdx2DBitArray);
    procedure AddCodeLevelAndMaskToMatrix(AMatrix: Tdx2DBitArray; const ACode: string);
    procedure AddDataToMatrix(ABinaryCode: TdxBitArray; ADataMatrixMask: Tdx2DBitArray; ADataMatrix: Tdx2DBitArray);
    procedure AddRequiredPatternsToMatrix(ARegularMatrix, ARegularMatrixMask: Tdx2DBitArray);
    procedure AddSearchPatternsToMatrix(ARegularMatrix, ARegularMatrixMask: Tdx2DBitArray);
    procedure AddVersionCodeToMatrix(AMatrix: Tdx2DBitArray);
    function GetDataMatrixMask(AMaskMatrixMask: Tdx2DBitArray): Tdx2DBitArray;
    function GetMaskMatrixMask(ARegularMatrixMask: Tdx2DBitArray): Tdx2DBitArray;
    function GetMatrixSize: Integer;

    procedure AssignMask(AMatrix, AMaskMatrixMask: Tdx2DBitArray; AMaskIndex: Integer);

    function SelectMask(AMatrix, AMaskMatrixMask: Tdx2DBitArray): Integer;
  protected
    FVersion: Integer;

    // TdxCustomBarCode
    function CalculateRect(const ACodeText: string; AFont: TFont; var ARect: TRect): TdxBarCodeErrorType; override;
    function CalculateSize(const AText: string; ASymbology: TdxCustomBarCodeSymbology; const AFont: TFont; out AWidth: Integer; out AHeight: Integer): TdxBarCodeErrorType; override;
    function CheckText(const AText: string): TdxBarCodeErrorType; override;
    procedure DrawBars(const ABoundsRect: TRect; const ACodeText: string; const ABarSize: TdxSizeF); override;
    function GetCodeWidth(const ACodeText: string): Integer; override;
    function GetInternalSize(const ACodeText: string; const AFont: TFont): TSize;  override;
    function Paint(APainter: TdxBarCodeCustomPainter; const ABounds: TRect; const AText: string; ASymbology: TdxCustomBarCodeSymbology; const AFont: TFont): TdxBarCodeErrorType; override;
    procedure DrawText(const ACodeText: string; const ARect: TRect; const ABarSize: TdxSizeF; AFont: TFont); override;

    function CreateBinaryMatrix(const ACodeText: string; AMaskIndex: Integer = -1; AMask: Boolean = True): Tdx2DBitArray;
    function GetMinVersion(const AText: string): Integer;
    function SelectMaskRule1(AMatrixWithMask: Tdx2DBitArray; X, Y: Integer; ANewLine: Boolean; var AMonochromeModuleCounter: Integer; var ACurrentModuleColor: TdxBit): Integer;
    function SelectMaskRule2(AMatrixWithMask: Tdx2DBitArray; X, Y: Integer): Integer;
    function SelectMaskRule3(AMatrixWithMask: Tdx2DBitArray; X, Y: Integer): Integer;

    function GetBottomOffset: Integer; override;
    function GetLeftOffset: Integer; override;
    function GetRightOffset: Integer; override;
    function GetTopOffset: Integer; override;

    property Symbology: TdxBarCodeQRCodeSymbology read GetSymbology;
  end;

  { TdxCustomBarCodeSymbology }

  TdxCustomBarCodeSymbology = class(TPersistent)
  strict private
    FFitMode: TdxBarCodeFitMode;
    FModuleColor: TColor;
    FModuleWidth: Integer;
    FShowText: Boolean;
    FOnChanged: TNotifyEvent;
  protected
    FGenerator: TdxCustomBarCodeGenerator;

    procedure Changed; virtual;
    function GetGeneratorClass: TdxCustomBarCodeGeneratorClass; virtual; abstract;

    function CalculateSize(const AText: string; const AFont: TFont; out AWidth: Integer; out AHeight: Integer): TdxBarCodeErrorType;
    function Paint(APainter: TdxBarCodeCustomPainter; const ABounds: TRect; const AText: string; const AFont: TFont): TdxBarCodeErrorType; overload;
    function Paint(ACanvas: TcxCanvas; const ABounds: TRect; const AText: string; const AFont: TFont): TdxBarCodeErrorType; overload;

    property FitMode: TdxBarCodeFitMode read FFitMode write FFitMode;
    property ModuleColor: TColor read FModuleColor write FModuleColor;
    property ModuleWidth: Integer read FModuleWidth write FModuleWidth;
    property ShowText: Boolean read FShowText write FShowText;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  end;

  { TdxBarCodeEAN13Symbology }

  TdxBarCodeEAN13Symbology = class(TdxCustomBarCodeSymbology)
  protected
    function GetGeneratorClass: TdxCustomBarCodeGeneratorClass; override;
  end;

  { TdxBarCodeEAN8Symbology }

  TdxBarCodeEAN8Symbology = class(TdxCustomBarCodeSymbology)
  protected
    function GetGeneratorClass: TdxCustomBarCodeGeneratorClass; override;
  end;

  { TdxBarCodeUPCASymbology }

  TdxBarCodeUPCASymbology = class(TdxCustomBarCodeSymbology)
  protected
    function GetGeneratorClass: TdxCustomBarCodeGeneratorClass; override;
  end;

  { TdxBarCodeUPCESymbology }

  TdxBarCodeUPCESymbology = class(TdxCustomBarCodeSymbology)
  protected
    function GetGeneratorClass: TdxCustomBarCodeGeneratorClass; override;
  end;

  { TdxBarCode128Symbology }

  TdxBarCode128Symbology = class(TdxCustomBarCodeSymbology)
  strict private
    FCharacterSet: TdxBarCode128CharacterSet;

    procedure SetCharacterSet(AValue: TdxBarCode128CharacterSet);
  protected
    function GetGeneratorClass: TdxCustomBarCodeGeneratorClass; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property CharacterSet: TdxBarCode128CharacterSet read FCharacterSet write SetCharacterSet;
  end;

  { TdxBarCodeITFTypeSymbology }

  TdxBarCodeITFTypeSymbology = class(TdxCustomBarCodeSymbology)
  strict private
    FChecksum: Boolean;
    FWideNarrowRatio: Single;

    procedure SetChecksum(AValue: Boolean);
    procedure SetWideNarrowRatio(AValue: Single);
  protected
    function IsWideNarrowRatioStored: Boolean; virtual;

    property Checksum: Boolean read FChecksum write SetChecksum;
    property WideNarrowRatio: Single read FWideNarrowRatio write SetWideNarrowRatio stored IsWideNarrowRatioStored;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
  end;

  { TdxBarCodeInterleaved2Of5Symbology }

  TdxBarCodeInterleaved2Of5Symbology = class(TdxBarCodeITFTypeSymbology)
  protected
    function GetGeneratorClass: TdxCustomBarCodeGeneratorClass; override;
  published
    property Checksum default True;
    property WideNarrowRatio;
  end;

  { TdxBarCode39Symbology }

  TdxBarCode39Symbology = class(TdxBarCodeITFTypeSymbology)
  protected
    function GetGeneratorClass: TdxCustomBarCodeGeneratorClass; override;
  published
    property Checksum default True;
    property WideNarrowRatio;
  end;

  { TdxBarCode39ExtendedSymbology }

  TdxBarCode39ExtendedSymbology = class(TdxBarCode39Symbology)
  protected
    function GetGeneratorClass: TdxCustomBarCodeGeneratorClass; override;
  end;

  { TdxBarCode93Symbology }

  TdxBarCode93Symbology = class(TdxCustomBarCodeSymbology)
  protected
    function GetGeneratorClass: TdxCustomBarCodeGeneratorClass; override;
  end;

  { TdxBarCode93ExtendedSymbology }

  TdxBarCode93ExtendedSymbology = class(TdxBarCode93Symbology)
  protected
    function GetGeneratorClass: TdxCustomBarCodeGeneratorClass; override;
  end;

  { TdxBarCodeMSISymbology }

  TdxBarCodeMSISymbology = class(TdxCustomBarCodeSymbology)
  protected
    function GetGeneratorClass: TdxCustomBarCodeGeneratorClass; override;
  end;

  { TdxBarCode11Symbology }

  TdxBarCode11Symbology = class(TdxBarCodeITFTypeSymbology)
  protected
    function GetGeneratorClass: TdxCustomBarCodeGeneratorClass; override;
  public
    constructor Create; override;
  end;

  { TdxBarCodeQRCodeSymbology }

  TdxBarCodeQRCodeSymbology = class(TdxCustomBarCodeSymbology)
  strict private
    FCompactionMode: TdxQRCodeCompactionMode;
    FErrorCorrectionLevel: TdxQRCodeErrorCorrectionLevel;
    FVersion: Integer;

    procedure SetCompactionMode(AValue: TdxQRCodeCompactionMode);
    procedure SetErrorCorrectionLevel(AValue: TdxQRCodeErrorCorrectionLevel);
    procedure SetVersion(AValue: Integer);
  protected
    function GetGeneratorClass: TdxCustomBarCodeGeneratorClass; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create; override;
  published
    property CompactionMode: TdxQRCodeCompactionMode read FCompactionMode write SetCompactionMode default qrmByte;
    property ErrorCorrectionLevel: TdxQRCodeErrorCorrectionLevel read FErrorCorrectionLevel write SetErrorCorrectionLevel default eclM;
    property Version: Integer read FVersion write SetVersion default 0;
  end;

function dxGetRegisteredBarCodeSymbologies: TcxRegisteredClasses;

implementation

uses
  Math,
  cxDrawTextUtils;

  {$REGION 'Constants'}

const

  FFirstNumberMapEAN13: array[0..9] of string =
      ('111111', '112122', '112212', '112221', '121122', '122112', '122211', '121212', '121221', '122121');

  FLastNumberMapUPCE: array[0..1, 0..9] of string = (
      ('222111', '221211', '221121', '221112', '212211', '211221', '211122', '212121', '212112', '211212'),
      ('111222', '112122', '112212', '112221', '121122', '122112', '122211', '121212', '121221', '122121'));

  FNumbersMapEAN: array [1..3, 0..9] of string = (
      ('0001101', '0011001', '0010011', '0111101', '0100011', '0110001', '0101111', '0111011', '0110111', '0001011'),
      ('0100111', '0110011', '0011011', '0100001', '0011101', '0111001', '0000101', '0010001', '0001001', '0010111'),
      ('1110010', '1100110', '1101100', '1000010', '1011100', '1001110', '1010000', '1000100', '1001000', '1110100'));

  FSymbolMap128: array[0..106, 0..3] of string = (
      (' ',               ' ',               '00',               '11011001100'),
      ('!',               '!',               '01',               '11001101100'),
      ('"',               '"',               '02',               '11001100110'),
      ('#',               '#',               '03',               '10010011000'),
      ('$',               '$',               '04',               '10010001100'),
      ('%',               '%',               '05',               '10001001100'),
      ('&',               '&',               '06',               '10011001000'),
      ('''',              '''',              '07',               '10011000100'),
      ('(',               '(',               '08',               '10001100100'),
      (')',               ')',               '09',               '11001001000'),
      ('*',               '*',               '10',               '11001000100'),
      ('+',               '+',               '11',               '11000100100'),
      (',',               ',',               '12',               '10110011100'),
      ('-',               '-',               '13',               '10011011100'),
      ('.',               '.',               '14',               '10011001110'),
      ('/',               '/',               '15',               '10111001100'),
      ('0',               '0',               '16',               '10011101100'),
      ('1',               '1',               '17',               '10011100110'),
      ('2',               '2',               '18',               '11001110010'),
      ('3',               '3',               '19',               '11001011100'),
      ('4',               '4',               '20',               '11001001110'),
      ('5',               '5',               '21',               '11011100100'),
      ('6',               '6',               '22',               '11001110100'),
      ('7',               '7',               '23',               '11101101110'),
      ('8',               '8',               '24',               '11101001100'),
      ('9',               '9',               '25',               '11100101100'),
      (':',               ':',               '26',               '11100100110'),
      (';',               ';',               '27',               '11101100100'),
      ('<',               '<',               '28',               '11100110100'),
      ('=',               '=',               '29',               '11100110010'),
      ('>',               '>',               '30',               '11011011000'),
      ('?',               '?',               '31',               '11011000110'),
      ('@',               '@',               '32',               '11000110110'),
      ('A',               'A',               '33',               '10100011000'),
      ('B',               'B',               '34',               '10001011000'),
      ('C',               'C',               '35',               '10001000110'),
      ('D',               'D',               '36',               '10110001000'),
      ('E',               'E',               '37',               '10001101000'),
      ('F',               'F',               '38',               '10001100010'),
      ('G',               'G',               '39',               '11010001000'),
      ('H',               'H',               '40',               '11000101000'),
      ('I',               'I',               '41',               '11000100010'),
      ('J',               'J',               '42',               '10110111000'),
      ('K',               'K',               '43',               '10110001110'),
      ('L',               'L',               '44',               '10001101110'),
      ('M',               'M',               '45',               '10111011000'),
      ('N',               'N',               '46',               '10111000110'),
      ('O',               'O',               '47',               '10001110110'),
      ('P',               'P',               '48',               '11101110110'),
      ('Q',               'Q',               '49',               '11010001110'),
      ('R',               'R',               '50',               '11000101110'),
      ('S',               'S',               '51',               '11011101000'),
      ('T',               'T',               '52',               '11011100010'),
      ('U',               'U',               '53',               '11011101110'),
      ('V',               'V',               '54',               '11101011000'),
      ('W',               'W',               '55',               '11101000110'),
      ('X',               'X',               '56',               '11100010110'),
      ('Y',               'Y',               '57',               '11101101000'),
      ('Z',               'Z',               '58',               '11101100010'),
      ('[',               '[',               '59',               '11100011010'),
      ('\',               '\',               '60',               '11101111010'),
      (']',               ']',               '61',               '11001000010'),
      ('^',               '^',               '62',               '11110001010'),
      ('_',               '_',               '63',               '10100110000'),
      (#0{NUL},           '`',               '64',               '10100001100'),
      (#1{SOH},           'a',               '65',               '10010110000'),
      (#2{STX},           'b',               '66',               '10010000110'),
      (#3{ETX},           'c',               '67',               '10000101100'),
      (#4{EOT},           'd',               '68',               '10000100110'),
      (#5{ENQ},           'e',               '69',               '10110010000'),
      (#6{ACK},           'f',               '70',               '10110000100'),
      (#7{BEL},           'g',               '71',               '10011010000'),
      (#8{BS},            'h',               '72',               '10011000010'),
      (#9{HT},            'i',               '73',               '10000110100'),
      (#10{LF},           'j',               '74',               '10000110010'),
      (#11{VT},           'k',               '75',               '11000010010'),
      (#12{FF},           'l',               '76',               '11001010000'),
      (#13{CR},           'm',               '77',               '11110111010'),
      (#14{SO},           'n',               '78',               '11000010100'),
      (#15{SI},           'o',               '79',               '10001111010'),
      (#16{DLE},          'p',               '80',               '10100111100'),
      (#17{DC1},          'q',               '81',               '10010111100'),
      (#18{DC2},          'r',               '82',               '10010011110'),
      (#19{DC3},          's',               '83',               '10111100100'),
      (#20{DC4},          't',               '84',               '10011110100'),
      (#21{NAK},          'u',               '85',               '10011110010'),
      (#22{SYN},          'v',               '86',               '11110100100'),
      (#23{ETB},          'w',               '87',               '11110010100'),
      (#24{CAN},          'x',               '88',               '11110010010'),
      (#25{EM},           'y',               '89',               '11011011110'),
      (#26{SUB},          'z',               '90',               '11011110110'),
      (#27{ESC},          '{',               '91',               '11110110110'),
      (#28{FS},           '|',               '92',               '10101111000'),
      (#29{GS},           '}',               '93',               '10100011110'),
      (#30{RS},           '~',               '94',               '10001011110'),
      (#31{US},           '' {DEL},          '95',               '10111101000'),
      ('' {FNC 3},        '' {FNC 3},        '96',               '10111100010'),
      ('' {FNC 2},        '' {FNC 2},        '97',               '11110101000'),
      ('' {Shift B},      '' {Shift A},      '98',               '11110100010'),
      ('' {CODE C},       '' {CODE C},       '99',               '10111011110'),
      ('' {CODE B},       '' {FNC 4},        ''  {CODE B},       '10111101110'),
      ('' {FNC 4},        '' {CODE A},       ''  {CODE A},       '11101011110'),
      ('' {FNC 1},        '' {FNC 1},        ''  {FNC 1},        '11110101110'),
      ('' {START CODE A}, '' {START CODE A}, ''  {START CODE A}, '11010000100'),
      ('' {START CODE B}, '' {START CODE B}, ''  {START CODE B}, '11010010000'),
      ('' {START CODE C}, '' {START CODE C}, ''  {START CODE C}, '11010011100'),
      ('' {STOP},         '' {STOP},         ''  {STOP},         '11000111010')
  );

  FSymbolMap_2_5: array[0..1, 0..9] of string = (
      ('0000101000', '1000000010', '0010000010', '1010000000', '0000100010',
       '1000100000', '0010100000', '0000001010', '1000001000', '0010001000'),
      ('0000010100', '0100000001', '0001000001', '0101000000', '0000010001',
       '0100010000', '0001010000', '0000000101', '0100000100', '0001000100'));

  FSymbolMapCode39: array[0..42, 0..1] of string =
      (('0', '000110100'), ('1', '100100001'), ('2', '001100001'), ('3', '101100000'), ('4', '000110001'),
       ('5', '100110000'), ('6', '001110000'), ('7', '000100101'), ('8', '100100100'), ('9', '001100100'),
       ('A', '100001001'), ('B', '001001001'), ('C', '101001000'), ('D', '000011001'), ('E', '100011000'),
       ('F', '001011000'), ('G', '000001101'), ('H', '100001100'), ('I', '001001100'), ('J', '000011100'),
       ('K', '100000011'), ('L', '001000011'), ('M', '101000010'), ('N', '000010011'), ('O', '100010010'),
       ('P', '001010010'), ('Q', '000000111'), ('R', '100000110'), ('S', '001000110'), ('T', '000010110'),
       ('U', '110000001'), ('V', '011000001'), ('W', '111000000'), ('X', '010010001'), ('Y', '110010000'),
       ('Z', '011010000'), ('-', '010000101'), ('.', '110000100'), (' ', '011000100'), ('$', '010101000'),
       ('/', '010100010'), ('+', '010001010'), ('%', '000101010'));

  FASCIIToCode39Symbols: array[0..127] of string =
      ('%U', '$A', '$B', '$C', '$D', '$E', '$F', '$G', '$H', '$I', '$J', '$K', '$L', '$M', '$N', '$O', '$P', '$Q', '$R',
       '$S', '$T', '$U', '$V', '$W', '$X', '$Y', '$Z', '%A', '%B', '%C', '%D', '%E', ' ', '/A', '/B', '/C', '/D', '/E',
       '/F', '/G', '/H', '/I', '/J', '/K', '/L', '/M', '/N', '/O', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
       '/Z', '%F', '%G', '%H', '%I', '%J', '%V', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
       'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '%K', '%L', '%M', '%N', '%O', '%W', '+A', '+B', '+C',
       '+D', '+E', '+F', '+G', '+H', '+I', '+J', '+K', '+L', '+M', '+N', '+O', '+P', '+Q', '+R', '+S', '+T', '+U', '+V',
       '+W', '+X', '+Y', '+Z', '%P', '%Q', '%R', '%S', '%T');

  FSymbolMapCode93: array[0..46, 0..1] of string =
      (('0', '100010100'), ('1', '101001000'), ('2', '101000100'), ('3', '101000010'), ('4', '100101000'),
       ('5', '100100100'), ('6', '100100010'), ('7', '101010000'), ('8', '100010010'), ('9', '100001010'),
       ('A', '110101000'), ('B', '110100100'), ('C', '110100010'), ('D', '110010100'), ('E', '110010010'),
       ('F', '110001010'), ('G', '101101000'), ('H', '101100100'), ('I', '101100010'), ('J', '100110100'),
       ('K', '100011010'), ('L', '101011000'), ('M', '101001100'), ('N', '101000110'), ('O', '100101100'),
       ('P', '100010110'), ('Q', '110110100'), ('R', '110110010'), ('S', '110101100'), ('T', '110100110'),
       ('U', '110010110'), ('V', '110011010'), ('W', '101101100'), ('X', '101100110'), ('Y', '100110110'),
       ('Z', '100111010'), ('-', '100101110'), ('.', '111010100'), (' ', '111010010'), ('$', '111001010'),
       ('/', '101101110'), ('+', '101110110'), ('%', '110101110'), (':', '100100110'), (';', '111011010'),
       ('<', '111010110'), ('=', '100110010'));

  FASCIIToCode93Symbols: array[0..127] of string =
      (';U', ':A', ':B', ':C', ':D', ':E', ':F', ':G', ':H', ':I', ':J', ':K', ':L', ':M', ':N', ':O', ':P', ':Q', ':R',
       ':S', ':T', ':U', ':V', ':W', ':X', ':Y', ':Z', ';A', ';B', ';C', ';D', ';E', ' ', '<A', '<B', '<C', '<D', '<E',
       '<F', '<G', '<H', '<I', '<J', '<K', '<L', '<M', '<N', '<O', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
       '<Z', ';F', ';G', ';H', ';I', ';J', ';V', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
       'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', ';K', ';L', ';M', ';N', ';O', ';W', '=A', '=B', '=C',
       '=D', '=E', '=F', '=G', '=H', '=I', '=J', '=K', '=L', '=M', '=N', '=O', '=P', '=Q', '=R', '=S', '=T', '=U', '=V',
       '=W', '=X', '=Y', '=Z', ';P', ';Q', ';R', ';S', ';T');

  FNumberMapMSI: array[0..9] of string =
      ('100100100100', '100100100110', '100100110100', '100100110110', '100110100100', '100110100110', '100110110100',
       '100110110110', '110100100100', '110100100110');

  FSymbolMapCode11: array[0..10] of string =
      ('000010', '100010', '010010', '110000', '001010', '101000', '011000', '000110', '100100', '100000', '001000');

  FNumberMapPostNet: array[0..9] of string =
      ('11000', '00011', '00101', '00110', '01001', '01010', '01100', '10001', '10010', '10100');

  FQRCodeAlignmentElementPositions: array[0..39, 0..6] of Integer =
    ((0, 0, 0, 0, 0, 0, 0), (6, 18, 0, 0, 0, 0, 0), (6, 22, 0, 0, 0, 0, 0),
     (6, 26, 0, 0, 0, 0, 0), (6, 30, 0, 0, 0, 0, 0), (6, 34, 0, 0, 0, 0, 0),
     (6, 22, 38, 0, 0, 0, 0), (6, 24, 42, 0, 0, 0, 0), (6, 26, 46, 0, 0, 0, 0),
     (6, 28, 50, 0, 0, 0, 0), (6, 30, 54, 0, 0, 0, 0), (6, 32, 58, 0, 0, 0, 0),
     (6, 34, 62, 0, 0, 0, 0), (6, 26, 48, 70, 0, 0, 0), (6, 26, 48, 70, 0, 0, 0),
     (6, 26, 50, 74, 0, 0, 0), (6, 30, 54, 78, 0, 0, 0), (6, 30, 56, 82, 0, 0, 0),
     (6, 30, 58, 86, 0, 0, 0), (6, 34, 62, 90, 0, 0, 0), (6, 28, 50, 72, 94, 0, 0),
     (6, 26, 50, 74, 98, 0, 0), (6, 30, 54, 78, 102, 0, 0), (6, 28, 54, 80, 106, 0, 0),
     (6, 32, 58, 84, 110, 0, 0), (6, 30, 58, 86, 114, 0, 0), (6, 34, 62, 90, 118, 0, 0),
     (6, 26, 50, 74, 98, 122, 0), (6, 30, 54, 78, 102, 126, 0), (6, 26, 52, 78, 104, 130, 0),
     (6, 30, 56, 82, 108, 134, 0), (6, 34, 60, 86, 112, 138, 0), (6, 30, 58, 86, 114, 142, 0),
     (6, 34, 62, 90, 118, 146, 0), (6, 30, 54, 78, 102, 126, 150), (6, 24, 50, 76, 102, 128, 154),
     (6, 28, 54, 80, 106, 132, 158), (6, 32, 58, 84, 110, 136, 162), (6, 26, 54, 82, 110, 138, 166),
     (6, 30, 58, 86, 114, 142, 170));

  FQRCodeDataWordCount: array[1..40, TdxQRCodeErrorCorrectionLevel] of Integer =
    ((19, 16, 13, 9), (34, 28, 22, 16), (55, 44, 34, 26), (80, 64, 48, 36), (108, 86, 62, 46), (136, 108, 76, 60),
     (156, 124, 88, 66), (194, 154, 110, 86), (232, 182, 132, 100), (274, 216, 154, 122), (324, 254, 180, 140),
     (370, 290, 206, 158), (428, 334, 244, 180), (461, 365, 261, 197), (523, 415, 295, 223), (589, 453, 325, 253),
     (647, 507, 367, 283), (721, 563, 397, 313), (795, 627, 445, 341), (861, 669, 485, 385), (932, 714, 512, 406),
     (1006, 782, 568, 442), (1094, 860, 614, 464), (1174, 914, 664, 514), (1276, 1000, 718, 538),
     (1370, 1062, 754, 596), (1468, 1128, 808, 628), (1531, 1193, 871, 661), (1631, 1267, 911, 701),
     (1735, 1373, 985, 745), (1843, 1455, 1033, 793), (1955, 1541, 1115, 845), (2071, 1631, 1171, 901),
     (2191, 1725, 1231, 961), (2306, 1812, 1286, 986), (2434, 1914, 1354, 1054), (2566, 1992, 1426, 1096),
     (2702, 2102, 1502, 1142), (2812, 2216, 1582, 1222), (2956, 2334, 1666, 1276));

  FQRCodeErrorCorrectionWordInBlockCount: array[1..40, TdxQRCodeErrorCorrectionLevel] of Integer =
    ((7, 10, 13, 17), (10, 16, 22, 28), (15, 26, 18, 22), (20, 18, 26, 16), (26, 24, 18, 22), (18, 16, 24, 28),
     (20, 18, 18, 26), (24, 22, 22, 26), (30, 22, 20, 24), (18, 26, 24, 28), (20, 30, 28, 24), (24, 22, 26, 28),
     (26, 22, 24, 22), (30, 24, 20, 24), (22, 24, 30, 24), (24, 28, 24, 30), (28, 28, 28, 28), (30, 26, 28, 28),
     (28, 26, 26, 26), (28, 26, 30, 28), (28, 26, 28, 30), (28, 28, 30, 24), (30, 28, 30, 30), (30, 28, 30, 30),
     (26, 28, 30, 30), (28, 28, 28, 30), (30, 28, 30, 30), (30, 28, 30, 30), (30, 28, 30, 30), (30, 28, 30, 30),
     (30, 28, 30, 30), (30, 28, 30, 30), (30, 28, 30, 30), (30, 28, 30, 30), (30, 28, 30, 30), (30, 28, 30, 30),
     (30, 28, 30, 30), (30, 28, 30, 30), (30, 28, 30, 30), (30, 28, 30, 30));

  FQRCodeBlockCount: array[1..40, TdxQRCodeErrorCorrectionLevel] of Integer =
    ((1, 1, 1, 1), (1, 1, 1, 1), (1, 1, 2, 2), (1, 2, 2, 4), (1, 2, 4, 4), (2, 4, 4, 4), (2, 4, 6, 5), (2, 4, 6, 6),
     (2, 5, 8, 8), (4, 5, 8, 8), (4, 5, 8, 11), (4, 8, 10, 11), (4, 9, 12, 16), (4, 9, 16, 16), (6, 10, 12, 18),
     (6, 10, 17, 16), (6, 11, 16, 19), (6, 13, 18, 21), (7, 14, 21, 25), (8, 16, 20 ,25), (8, 17, 23, 25),
     (9, 17, 23, 34), (9, 18, 25, 30), (10, 20, 27, 32), (12, 21, 29, 35), (12, 23, 34, 37), (12, 25, 34, 40),
     (13, 26, 35, 42), (14, 28, 38, 45), (15, 29, 40, 48), (16, 31, 43, 51), (17, 33, 45, 54), (18, 35, 48, 57),
     (19, 37, 51, 60), (19, 38, 53, 63), (20, 40, 56, 66), (21, 43, 59, 70), (22, 45, 62, 74), (24, 47, 65, 77),
     (25, 49, 68, 81));

  FQRCodeGenerationPolynomial: array[7..30, 0..29] of Integer =
    ((87, 229, 146, 149, 238, 102, 21, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (251, 67, 46, 61, 118, 70, 64, 94, 32, 45, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (74, 152, 176, 100, 86, 100, 106, 104, 130, 218, 206, 140, 78, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (8, 183, 61, 91, 202, 37, 51, 58, 58, 237, 140, 124, 5, 99, 105, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (120, 104, 107, 109, 102, 161, 76, 3, 91, 191, 147, 169, 182, 194, 225, 120, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (43, 139, 206, 78, 43, 239, 123, 206, 214, 147, 24, 99, 150, 39, 243, 163, 136, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (215, 234, 158, 94, 184, 97, 118, 170, 79, 187, 152, 148, 252, 179, 5, 98, 96, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (17, 60, 79, 50, 61, 163, 26, 187, 202, 180, 221, 225, 83, 239, 156, 164, 212, 212, 188, 190, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (210, 171, 247, 242, 93, 230, 14, 109, 221, 53, 200, 74, 8, 172, 98, 80, 219, 134, 160, 105, 165, 231, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (229, 121, 135, 48, 211, 117, 251, 126, 159, 180, 169, 152, 192, 226, 228, 218, 111, 0, 117, 232, 87, 96, 227, 21, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (173, 125, 158, 2, 103, 182, 118, 17, 145, 201, 111, 28, 165, 53, 161, 21, 245, 142, 13, 102, 48, 227, 153, 145, 218, 70, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (168, 223, 200, 104, 224, 234, 108, 180, 110, 190, 195, 147, 205, 27, 232, 201, 21, 43, 245, 87, 42, 195, 212, 119, 242, 37, 9, 123, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (41, 173, 145, 152, 216, 31, 179, 182, 50, 48, 110, 86, 239, 96, 222, 125, 42, 173, 226, 193, 224, 130, 156, 37, 251, 216, 238, 40, 192, 180));

  FQRCodeGaloisField: array[0..255] of Integer = (1, 2, 4, 8, 16, 32, 64, 128, 29, 58, 116, 232, 205, 135, 19, 38, 76,
    152, 45, 90, 180, 117, 234, 201, 143, 3, 6, 12, 24, 48, 96, 192, 157, 39, 78, 156, 37, 74, 148, 53, 106, 212, 181,
    119, 238, 193, 159, 35, 70, 140, 5, 10, 20, 40, 80, 160, 93, 186, 105, 210, 185, 111, 222, 161, 95, 190, 97, 194,
    153, 47, 94, 188, 101, 202, 137, 15, 30, 60, 120, 240, 253, 231, 211, 187, 107, 214, 177, 127, 254, 225, 223, 163,
    91, 182, 113, 226, 217, 175, 67, 134, 17, 34, 68, 136, 13, 26, 52, 104, 208, 189, 103, 206, 129, 31, 62, 124, 248,
    237, 199, 147, 59, 118, 236, 197, 151, 51, 102, 204, 133, 23, 46, 92, 184, 109, 218, 169, 79, 158, 33, 66, 132, 21,
    42, 84, 168, 77, 154, 41, 82, 164, 85, 170, 73, 146, 57, 114, 228, 213, 183, 115, 230, 209, 191, 99, 198, 145, 63,
    126, 252, 229, 215, 179, 123, 246, 241, 255, 227, 219, 171, 75, 150, 49, 98, 196, 149, 55, 110, 220, 165, 87, 174,
    65, 130, 25, 50, 100, 200, 141, 7, 14, 28, 56, 112, 224, 221, 167, 83, 166, 81, 162, 89, 178, 121, 242, 249, 239,
    195, 155, 43, 86, 172, 69, 138, 9, 18, 36, 72, 144, 61, 122, 244, 245, 247, 243, 251, 235, 203, 139, 11, 22, 44, 88,
    176, 125, 250, 233, 207, 131, 27, 54, 108, 216, 173, 71, 142, 1);

  FQRCodeInverseGaloisField: array[1..255] of Integer = (0, 1, 25, 2, 50, 26, 198, 3, 223, 51, 238, 27, 104, 199, 75, 4,
    100, 224, 14, 52, 141, 239, 129, 28, 193, 105, 248, 200, 8, 76, 113, 5, 138, 101, 47, 225, 36, 15, 33, 53, 147, 142,
    218, 240, 18, 130, 69, 29, 181, 194, 125, 106, 39, 249, 185, 201, 154, 9, 120, 77, 228, 114, 166, 6, 191, 139, 98,
    102, 221, 48, 253, 226, 152, 37, 179, 16, 145, 34, 136, 54, 208, 148, 206, 143, 150, 219, 189, 241, 210, 19, 92,
    131, 56, 70, 64, 30, 66, 182, 163, 195, 72, 126, 110, 107, 58, 40, 84, 250, 133, 186, 61, 202, 94, 155, 159, 10, 21,
    121, 43, 78, 212, 229, 172, 115, 243, 167, 87, 7, 112, 192, 247, 140, 128, 99, 13, 103, 74, 222, 237, 49, 197, 254,
    24, 227, 165, 153, 119, 38, 184, 180, 124, 17, 68, 146, 217, 35, 32, 137, 46, 55, 63, 209, 91, 149, 188, 207, 205,
    144, 135, 151, 178, 220, 252, 190, 97, 242, 86, 211, 171, 20, 42, 93, 158, 132, 60, 57, 83, 71, 109, 65, 162, 31,
    45, 67, 216, 183, 123, 164, 118, 196, 23, 73, 236, 127, 12, 111, 246, 108, 161, 59, 82, 41, 157, 85, 170, 251, 96,
    134, 177, 187, 204, 62, 90, 203, 89, 95, 176, 156, 169, 160, 81, 11, 245, 22, 235, 122, 117, 44, 215, 79, 174, 213,
    233, 230, 231, 173, 232, 116, 214, 244, 234, 168, 80, 88, 175);

  FQRCodeMaskAndErrorLevelCode: array[TdxQRCodeErrorCorrectionLevel, 0..7] of string =
    (('111011111000100', '111001011110011', '111110110101010', '111100010011101',
      '110011000101111', '110001100011000', '110110001000001', '110100101110110'),
     ('101010000010010', '101000100100101', '101111001111100', '101101101001011',
      '100010111111001', '100000011001110', '100111110010111', '100101010100000'),
     ('011010101011111', '011000001101000', '011111100110001', '011101000000110',
      '010010010110100', '010000110000011', '010111011011010', '010101111101101'),
     ('001011010001001', '001001110111110', '001110011100111', '001100111010000',
      '000011101100010', '000001001010101', '000110100001100', '000100000111011'));

  FQRCodeVersionCode: array[7..40] of string =
    ('000010011110100110', '010001011100111000', '110111011000000100', '101001111110000000', '001111111010111100',
     '001101100100011010', '101011100000100110', '110101000110100010', '010011000010011110', '011100010001011100',
     '111010010101100000', '100100110011100100', '000010110111011000', '000000101001111110', '100110101101000010',
     '111000001011000110', '011110001111111010', '001101001101100100', '101011001001011000', '110101101111011100',
     '010011101011100000', '010001110101000110', '110111110001111010', '101001010111111110', '001111010011000010',
     '101000011000101101', '001110011100010001', '010000111010010101', '110110111110101001', '110100100000001111',
     '010010100100110011', '001100000010110111', '101010000110001011', '111001000100010101');

  FSymbolMapQRCodeAlphaNumeric: string = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:';

  QRCodeModeIndicator: array[TdxQRCodeCompactionMode] of string = ('0001', '0010', '0100');

  QRCodeNumberCountToBitCount: array[0..3] of Integer = (0, 4, 7, 10);
  QRCodeGroupCharCount = 3;
  QRCodeWordLength = 8;
  QRCodeLimiter: string = '0000';
  QRCodeFillSymbols: array[0..1] of string = ('11101100', '00010001');

{$ENDREGION}

var
  FRegisteredBarCodeSymbologies: TcxRegisteredClasses;

function dxGetRegisteredBarCodeSymbologies: TcxRegisteredClasses;
begin
  if FRegisteredBarCodeSymbologies = nil then
  begin
    FRegisteredBarCodeSymbologies := TcxRegisteredClasses.Create;
    FRegisteredBarCodeSymbologies.Sorted := True;
    FRegisteredBarCodeSymbologies.Register(TdxBarCodeEAN13Symbology, 'CodeEAN13');
    FRegisteredBarCodeSymbologies.Register(TdxBarCodeEAN8Symbology, 'CodeEAN8');
    FRegisteredBarCodeSymbologies.Register(TdxBarCodeUPCASymbology, 'CodeUPCA');
    FRegisteredBarCodeSymbologies.Register(TdxBarCodeUPCESymbology, 'CodeUPCE');
    FRegisteredBarCodeSymbologies.Register(TdxBarCode128Symbology, 'Code128');
    FRegisteredBarCodeSymbologies.Register(TdxBarCodeInterleaved2Of5Symbology, 'Interleaved2Of5');
    FRegisteredBarCodeSymbologies.Register(TdxBarCode39Symbology, 'Code39');
    FRegisteredBarCodeSymbologies.Register(TdxBarCode39ExtendedSymbology, 'Code39Extended');
    FRegisteredBarCodeSymbologies.Register(TdxBarCode93Symbology, 'Code93');
    FRegisteredBarCodeSymbologies.Register(TdxBarCode93ExtendedSymbology, 'Code93Extended');
    FRegisteredBarCodeSymbologies.Register(TdxBarCodeMSISymbology, 'CodeMSI');
    FRegisteredBarCodeSymbologies.Register(TdxBarCode11Symbology, 'Code11');
    FRegisteredBarCodeSymbologies.Register(TdxBarCodeQRCodeSymbology, 'QRCode');
  end;
  Result := FRegisteredBarCodeSymbologies;
end;

{ TdxBarCodePainter }

procedure TdxBarCodePainter.DrawText(const ARect: TRect; const AText: string; AFormat: Integer; AFont: TFont);
var
  ATextRect: TRect;
begin
  ATextRect := ARect;
  Canvas.SaveState;
  try
    Canvas.Font.Assign(AFont);
    cxDrawText(Canvas.Handle, AText, ATextRect, AFormat);
  finally
    Canvas.RestoreState;
  end;
end;

procedure TdxBarCodePainter.FillRect(const ARect: TRect; AColor: TColor);
begin
  Canvas.FillRect(ARect, AColor);
end;

{ TdxCustomBarCodeGenerator }

function TdxCustomBarCodeGenerator.CalculateSize(const AText: string; ASymbology: TdxCustomBarCodeSymbology;
  const AFont: TFont; out AWidth: Integer; out AHeight: Integer): TdxBarCodeErrorType;
begin
  Result := bceNone;
end;

function TdxCustomBarCodeGenerator.Paint(APainter: TdxBarCodeCustomPainter; const ABounds: TRect;
  const AText: string; ASymbology: TdxCustomBarCodeSymbology; const AFont: TFont): TdxBarCodeErrorType;

  function GetBarSize(const ACodeText: string; AScaledFont: TFont): TdxSizeF;
  begin
    Result.cx := Symbology.ModuleWidth * ScaleX(cxRectSize(ABounds), GetInternalSize(ACodeText, AFont));
    Result.cy := DefaultBarHeight * ScaleY(cxRectSize(ABounds), GetInternalSize(ACodeText, AFont));

    if Symbology.ShowText and (Symbology.FitMode = ifmStretch) then
      Result.cy := cxRectHeight(ABounds) - cxTextHeight(AScaledFont) - (GetBottomOffset + GetTopOffset) * Result.cx;
  end;

var
  ACodeText: string;
  ACodeRect: TRect;
  ARect: TRect;
  AScaledFont: TFont;
begin
  FSymbology := ASymbology;
  FPainter := APainter;
  Result := CheckText(AText);
  if Result = bceNone then
  begin
    ACodeText := GetCodeText(AText);
    ARect := ABounds;
    Result := CalculateRect(ACodeText, AFont, ARect);
    if Result = bceNone then
    begin
      AScaledFont := TFont.Create;
      try
        AScaledFont.Assign(AFont);
        AScaledFont.Orientation := 0;
        ACodeRect := cxRectCenterHorizontally(ARect, Round(GetCodeWidth(ACodeText) *
          ScaleX(cxRectSize(ABounds), GetInternalSize(ACodeText, AScaledFont))));
        AScaledFont.Height := GetScaledFontHeight(ABounds, AScaledFont, ACodeText);
        AdjustFontSize(GetTextForDrawing(ACodeText), cxRectWidth(ARect), AScaledFont);
        DrawBars(ACodeRect, ACodeText, GetBarSize(ACodeText, AScaledFont));
        if FSymbology.ShowText then
          DrawText(ACodeText, GetTextRect(ABounds, ARect), GetBarSize(ACodeText, AScaledFont), AScaledFont);
      finally
        AScaledFont.Free;
      end;
    end;
  end;
end;

procedure TdxCustomBarCodeGenerator.AdjustCodeText(var ACodeText: string);
begin
// do nothing
end;

function TdxCustomBarCodeGenerator.CheckText(const AText: string): TdxBarCodeErrorType;
begin
  Result := bceNone;
end;

function TdxCustomBarCodeGenerator.GetCodeText(const AText: string): string;
begin
  Result := AText;
  AdjustCodeText(Result);
end;

function TdxCustomBarCodeGenerator.GetTextForDrawing(const ACodeText: string): string;
begin
  Result := ACodeText;
end;

function TdxCustomBarCodeGenerator.CalculateRect(const ACodeText: string; AFont: TFont;
  var ARect: TRect): TdxBarCodeErrorType;
begin
  Result := bceNone;
end;

function TdxCustomBarCodeGenerator.DefaultBarHeight: Integer;
begin
  Result := Symbology.ModuleWidth;
end;

function TdxCustomBarCodeGenerator.GetCodeWidth(const ACodeText: string): Integer;
begin
  Result := 0;
end;

function TdxCustomBarCodeGenerator.GetInternalSize(const ACodeText: string; const AFont: TFont): TSize;
begin
  Result.cx := GetCodeWidth(ACodeText);
  if Symbology.ShowText then
    Result.cy := DefaultBarHeight + GetTextHeight(GetTextForDrawing(ACodeText), AFont)
  else
    Result.cy := DefaultBarHeight;

  Result.cy := Result.cy + (GetTopOffset + GetBottomOffset) * Symbology.ModuleWidth;
end;

function TdxCustomBarCodeGenerator.GetScaledFontHeight(const ABounds: TRect; AFont: TFont;
  const ACodeText: string): Integer;
begin
  Result := Trunc(AFont.Height * ScaleX(cxRectSize(ABounds), GetInternalSize(ACodeText, AFont)));
end;

function TdxCustomBarCodeGenerator.GetTextHeight(const AText: string; AFont: TFont): Integer;
var
  ARect: TRect;
begin
  ARect := cxRectSetWidth(cxNullRect, cxTextWidth(AFont, AText));
  cxGetTextRect(ARect, AText, AFont, DT_TOP or DT_CENTER);
  Result := cxRectHeight(ARect);
end;

function TdxCustomBarCodeGenerator.GetTextRect(const ABoundsRect, ACodeRect: TRect): TRect;
begin
  Result := ACodeRect;
  Result.Left := ABoundsRect.Left;
  Result.Right := ABoundsRect.Right;
end;

function TdxCustomBarCodeGenerator.ScaleX(const AClientSize, ADefaultSize: TSize): Single;
begin
  case Symbology.FitMode of
    ifmStretch:
      Result := AClientSize.cx / ADefaultSize.cx;
    ifmProportionalStretch:
      Result := Min(AClientSize.cx / ADefaultSize.cx, AClientSize.cy / ADefaultSize.cy);
    ifmFit:
      Result := Min(1, Min(AClientSize.cx / ADefaultSize.cx, AClientSize.cy / ADefaultSize.cy));
  else
    Result := 1;
  end;
end;

function TdxCustomBarCodeGenerator.ScaleY(const AClientSize, ADefaultSize: TSize): Single;
begin
  case Symbology.FitMode of
    ifmStretch:
      Result := AClientSize.cy / ADefaultSize.cy;
    ifmProportionalStretch:
      Result := Min(AClientSize.cx / ADefaultSize.cx, AClientSize.cy / ADefaultSize.cy);
    ifmFit:
      Result := Min(1, Min(AClientSize.cx / ADefaultSize.cx, AClientSize.cy / ADefaultSize.cy));
  else
    Result := 1;
  end;
end;

procedure TdxCustomBarCodeGenerator.DrawBars(const ABoundsRect: TRect; const ACodeText: string; const ABarSize: TdxSizeF);
begin
// do nothing
end;

procedure TdxCustomBarCodeGenerator.DrawText(const ACodeText: string; const ARect: TRect; const ABarSize: TdxSizeF; AFont: TFont);
begin
// do nothing
end;

function TdxCustomBarCodeGenerator.GetBottomOffset: Integer;
begin
  Result := 0;
end;

function TdxCustomBarCodeGenerator.GetLeftOffset: Integer;
begin
  Result := 0;
end;

function TdxCustomBarCodeGenerator.GetRightOffset: Integer;
begin
  Result := 0;
end;

function TdxCustomBarCodeGenerator.GetTopOffset: Integer;
begin
  Result := 0;
end;

procedure TdxCustomBarCodeGenerator.AdjustFontSize(const AText: string; AAvailableWidth: Integer; AFont: TFont);
var
  AAdjustFontSizeHelper: TdxAdjustFontSizeHelper;
begin
  AAdjustFontSizeHelper := TdxAdjustFontSizeHelper.Create;
  try
    AAdjustFontSizeHelper.Font.Assign(AFont);
    AAdjustFontSizeHelper.Calculate(AAvailableWidth, AText);
    AFont.Size := AAdjustFontSizeHelper.Font.Size;
  finally
    AAdjustFontSizeHelper.Free;
  end;
end;

function TdxCustomBarCodeGenerator.GetSymbology: TdxCustomBarCodeSymbology;
begin
  Result := FSymbology;
end;

{ TdxBarCodeOneDimensionalGenerator }

function TdxBarCodeOneDimensionalGenerator.CalculateRect(const ACodeText: string;
  AFont: TFont; var ARect: TRect): TdxBarCodeErrorType;
var
  ABarCodeSize: TSize;
begin
  Result := bceNone;
  ABarCodeSize := GetInternalSize(ACodeText, AFont);
  ABarCodeSize.cx := Round(ABarCodeSize.cx * ScaleX(cxRectSize(ARect), GetInternalSize(ACodeText, AFont)));
  ABarCodeSize.cy := Round(ABarCodeSize.cy * ScaleY(cxRectSize(ARect), GetInternalSize(ACodeText, AFont)));
  if (ABarCodeSize.cy > cxRectHeight(ARect)) or (ABarCodeSize.cx > cxRectWidth(ARect)) then
    Result := bceBoundsTooSmall;

  ARect := cxRectCenter(ARect, ABarCodeSize);
end;

function TdxBarCodeOneDimensionalGenerator.CalculateSize(const AText: string; ASymbology: TdxCustomBarCodeSymbology;
  const AFont: TFont; out AWidth: Integer; out AHeight: Integer): TdxBarCodeErrorType;
var
  ASize: TSize;
begin
  inherited;
  FSymbology := ASymbology;
  try
    ASize := cxSize(100, 40);
    Result := CheckText(AText);
    if Result = bceNone then
    begin
      ASize := GetInternalSize(GetCodeText(AText), AFont);
      AWidth := Max(ASize.cx, cxTextWidth(AFont, GetTextForDrawing(GetCodeText(AText))));
      AHeight := ASize.cy;
    end;
  finally
    FSymbology := nil;
  end;
end;

function TdxBarCodeOneDimensionalGenerator.DefaultBarHeight: Integer;
begin
  Result := 32 * Symbology.ModuleWidth;
end;

procedure TdxBarCodeOneDimensionalGenerator.DrawText(const ACodeText: string; const ARect: TRect;
  const ABarSize: TdxSizeF; AFont: TFont);
var
  ATextRect: TRect;
begin
  ATextRect := ARect;
  ATextRect.Top := Trunc(ATextRect.Top + ABarSize.cy);
  ATextRect.Bottom := Round(ARect.Bottom - GetBottomOffset * ABarSize.cx);
  FPainter.DrawText(ATextRect, GetTextForDrawing(ACodeText), DT_SINGLELINE or DT_CENTER or DT_TOP, AFont);
end;

function TdxBarCodeOneDimensionalGenerator.GetBinaryCode(const ACodeText: string): TdxBitArray;
var
  I: Integer;
  AModulePos: Integer;
begin
  SetLength(Result, GetBinaryCodeLength(ACodeText));
  AModulePos := Length(GetStartBinaryCode);
  for I := 0 to AModulePos - 1 do
    Result[I] := TdxBit(GetStartBinaryCode[I + 1] = '1');

  for I := 1 to GetFullSymbolicCodeLength(ACodeText) do
    PopulatePartBinaryCode(GetFullSymbolicCode(ACodeText), Result, I, AModulePos);

  for I := AModulePos to AModulePos + Length(GetStopBinaryCode) - 1 do
    Result[I] := TdxBit(GetStopBinaryCode[I - AModulePos + 1] = '1');
end;

function TdxBarCodeOneDimensionalGenerator.GetBinaryCodeLength(const ACodeText: string): Integer;
begin
  Result := GetFullSymbolicCodeLength(ACodeText) * GetModuleForSymbolCount +
    Length(GetStartBinaryCode) + Length(GetStopBinaryCode);
end;

function TdxBarCodeOneDimensionalGenerator.GetFullSymbolicCode(const ACodeText: string): string;
begin
  Result := ACodeText;
end;

function TdxBarCodeOneDimensionalGenerator.GetFullSymbolicCodeLength(const ACodeText: string): Integer;
begin
  Result := Length(GetFullSymbolicCode(ACodeText));
end;

function TdxBarCodeOneDimensionalGenerator.GetModuleForSymbolCount: Integer;
begin
  Result := 0;
end;

function TdxBarCodeOneDimensionalGenerator.GetStartBinaryCode: string;
begin
  Result := '';
end;

function TdxBarCodeOneDimensionalGenerator.GetStopBinaryCode: string;
begin
  Result := '';
end;

procedure TdxBarCodeOneDimensionalGenerator.PopulatePartBinaryCode(const AFullSymbolicCode: string;
  ABinaryCode: TdxBitArray; ANumberPos: Integer; var AModulePos: Integer);
begin
// do nothing
end;

function TdxBarCodeOneDimensionalGenerator.GetSymbology: TdxCustomBarCodeSymbology;
begin
  Result := TdxCustomBarCodeSymbology(FSymbology);
end;

{ TdxBarCodeEANTypeGenerator }

procedure TdxBarCodeEANTypeGenerator.DrawBars(const ABoundsRect: TRect; const ACodeText: string; const ABarSize: TdxSizeF);
var
  I: Integer;
  ALineRect: TdxRectF;
  ACurrentPosition: TdxPointF;
  ALongLinePatternMask: TdxBitArray;
  ABinaryCode: TdxBitArray;
begin
  ALongLinePatternMask := GetLongLinePatternMask(ACodeText);
  ACurrentPosition.X := ABoundsRect.Left + GetLeftOffset * ABarSize.cx;
  ACurrentPosition.Y := ABoundsRect.Top + GetTopOffset * ABarSize.cx;
  ABinaryCode := GetBinaryCode(ACodeText);
  for I := 0 to Length(ABinaryCode) - 1 do
  begin
    ALineRect.TopLeft := ACurrentPosition;
    ALineRect.Right := ALineRect.Left + ABarSize.cx;
    ALineRect.Bottom := ALineRect.Top + ABarSize.cy;
    if (ALongLinePatternMask[I] = bit1) and Symbology.ShowText then
      ALineRect.Bottom := ALineRect.Top + GetLongBarHeight(ABarSize.cx, ABarSize.cy);
    if ABinaryCode[I] = bit1 then
      FPainter.FillRect(Rect(Round(ALineRect.Left), Round(ALineRect.Top), Round(ALineRect.Right), Round(ALineRect.Bottom)),
        Symbology.ModuleColor);
    ACurrentPosition.X := ACurrentPosition.X + ABarSize.cx;
  end;
end;

function TdxBarCodeEANTypeGenerator.GetCodeWidth(const ACodeText: string): Integer;
begin
  Result := Symbology.ModuleWidth * (Length(GetBinaryCode(ACodeText)) + GetLeftOffset + GetRightOffset);
end;

function TdxBarCodeEANTypeGenerator.GetLongBarHeight(ABarWidth, ABarHeight: Single): Single;
begin
  Result := ABarHeight;
end;

function TdxBarCodeEANTypeGenerator.GetLongLinePatternMask(const ACodeText: string): TdxBitArray;
begin
  SetLength(Result, GetBinaryCodeLength(ACodeText));
end;

{ TdxBarCodeEAN13Generator }

procedure TdxBarCodeEAN13Generator.AdjustCodeText(var ACodeText: string);
var
  I: Integer;
begin
  inherited;
  for I := Length(ACodeText) to 11 do
    ACodeText := '0' + ACodeText;
  if Length(ACodeText) > 12 then
    ACodeText := Copy(ACodeText, 1, 12);
  ACodeText := ACodeText + GetChecksum(ACodeText);
end;

function TdxBarCodeEAN13Generator.CheckText(const AText: string): TdxBarCodeErrorType;
var
  I: Integer;
begin
  Result := inherited CheckText(AText);
  if Result = bceNone then
    for I := 1 to Length(AText) do
      if not dxCharInSet(AText[I], ['0'..'9']) then
      begin
        Result := bceInvalidCharacters;
        Break;
      end;
end;

function TdxBarCodeEAN13Generator.DefaultBarHeight: Integer;
begin
  Result := Symbology.ModuleWidth * 70;
end;

procedure TdxBarCodeEAN13Generator.DrawText(const ACodeText: string; const ARect: TRect; const ABarSize: TdxSizeF; AFont: TFont);
var
  ATextRect: TRect;
  AText: string;
begin
  AText := Copy(GetTextForDrawing(ACodeText), 1, 1);
  ATextRect.Top := Trunc(ARect.Top + ABarSize.cy);
  ATextRect.Bottom := Round(ARect.Bottom - GetBottomOffset * ABarSize.cx);
  ATextRect.Left := ARect.Left;
  ATextRect.Right := Round(ARect.Left + (GetLeftOffset - 1) * ABarSize.cx);
  FPainter.DrawText(ATextRect, AText, DT_SINGLELINE or DT_LEFT or DT_TOP, AFont);

  AText := Copy(GetTextForDrawing(ACodeText), 2, 6);
  ATextRect.Left := Round(ARect.Left + GetLeftOffset * ABarSize.cx + 3 * ABarSize.cx);
  ATextRect.Right := Round(ATextRect.Left + (7 * 6 + 1) * ABarSize.cx);
  FPainter.DrawText(ATextRect, AText, DT_SINGLELINE or DT_CENTER or DT_TOP, AFont);

  AText := Copy(GetTextForDrawing(ACodeText), 8, 6);
  ATextRect.Left := Round(ATextRect.Right + 3 * ABarSize.cx);
  ATextRect.Right := Round(ATextRect.Left + (7 * 6 + 1) * ABarSize.cx);
  FPainter.DrawText(ATextRect, AText, DT_SINGLELINE or DT_CENTER or DT_TOP, AFont);
end;

function TdxBarCodeEAN13Generator.GetScaledFontHeight(const ABounds: TRect; AFont: TFont; const ACodeText: string): Integer;
begin
  Result := -Trunc(Symbology.ModuleWidth * ScaleX(cxRectSize(ABounds), GetInternalSize(ACodeText, AFont)) * 8.3);
  Result := MulDiv(Result, 96, 72);
end;

function TdxBarCodeEAN13Generator.GetTextForDrawing(const ACodeText: string): string;
begin
  Result := GetFullSymbolicCode(ACodeText);
end;

function TdxBarCodeEAN13Generator.GetTextRect(const ABoundsRect, ACodeRect: TRect): TRect;
begin
  Result := ACodeRect;
end;

function TdxBarCodeEAN13Generator.GetBinaryCodeLength(const ACodeText: string): Integer;
begin
  Result := inherited GetBinaryCodeLength(ACodeText) + Length(GetCenterCode) - GetModuleForSymbolCount;
end;

function TdxBarCodeEAN13Generator.GetFullSymbolicCode(const ACodeText: string): string;
begin
  Result := ACodeText;
  if Length(Result) = 12 then
    Result := Result + GetChecksum(ACodeText);
end;

function TdxBarCodeEAN13Generator.GetLongLinePatternMask(const ACodeText: string): TdxBitArray;
begin
  SetLength(Result, GetBinaryCodeLength(ACodeText));
  Result[0] := bit1;
  Result[2] := bit1;
  Result[46] := bit1;
  Result[48] := bit1;
  Result[92] := bit1;
  Result[94] := bit1;
end;

function TdxBarCodeEAN13Generator.GetLongBarHeight(ABarWidth, ABarHeight: Single): Single;
begin
  Result := ABarHeight + 5 * ABarWidth;
end;

function TdxBarCodeEAN13Generator.GetModuleForSymbolCount: Integer;
begin
  Result := 7;
end;

function TdxBarCodeEAN13Generator.GetStartBinaryCode: string;
begin
  Result := '101';
end;

function TdxBarCodeEAN13Generator.GetStopBinaryCode: string;
begin
  Result := '101';
end;

procedure TdxBarCodeEAN13Generator.PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray; ANumberPos: Integer;
  var AModulePos: Integer);
var
  ANumberMap: string;
  ANumberMapIndex: Integer;
  I: Integer;
begin
  if ANumberPos > 1 then
  begin
    if ANumberPos = 8 then
    begin
      for I := AModulePos to AModulePos + Length(GetCenterCode) - 1 do
        ABinaryCode[I] := TdxBit(GetCenterCode[I - AModulePos + 1] = '1');
      AModulePos := AModulePos + Length(GetCenterCode);
    end;

    if ANumberPos < 8 then
      ANumberMapIndex := StrToInt(FFirstNumberMapEAN13[StrToInt(AFullSymbolicCode[1])][ANumberPos - 1])
    else
      ANumberMapIndex := 3;

    ANumberMap := FNumbersMapEAN[ANumberMapIndex][StrToInt(AFullSymbolicCode[ANumberPos])];
    for I := AModulePos to AModulePos + 6 do
      ABinaryCode[I] := TdxBit(ANumberMap[I - AModulePos + 1] = '1');

    AModulePos := AModulePos + 7;
  end;
end;

function TdxBarCodeEAN13Generator.GetLeftOffset: Integer;
begin
  Result := dxBarCodeEAN13LeftQuietZoneSize;
end;

function TdxBarCodeEAN13Generator.GetRightOffset: Integer;
begin
  Result := dxBarCodeEAN13RightQuietZoneSize;
end;

function TdxBarCodeEAN13Generator.GetTextHeight(const AText: string; AFont: TFont): Integer;
var
  AInternalFont: TFont;
begin
  AInternalFont := TFont.Create;
  try
    AInternalFont.Assign(AFont);
    AInternalFont.Height := -MulDiv(Trunc(Symbology.ModuleWidth * 8.3), 96, 72);
    AdjustFontSize('00000', (7 * 6 + 1) * Symbology.ModuleWidth, AInternalFont);
    Result := inherited GetTextHeight(AText, AInternalFont);
  finally
    AInternalFont.Free;
  end;
end;

function TdxBarCodeEAN13Generator.GetCenterCode: string;
begin
  Result := '01010';
end;

function TdxBarCodeEAN13Generator.GetChecksum(const ACodeText: string): string;
var
  AChecksum: Integer;
  I: Integer;
begin
  AChecksum := 0;
  for I := 1 to 12 do
    if Odd(I) then
      AChecksum := AChecksum + StrToInt(ACodeText[I])
    else
      AChecksum := AChecksum + StrToInt(ACodeText[I]) * 3;

  AChecksum := (10 - AChecksum mod 10) mod 10;
  Result := IntToStr(AChecksum);
end;

function TdxBarCodeEAN13Generator.GetSymbology: TdxBarCodeEAN13Symbology;
begin
  Result := TdxBarCodeEAN13Symbology(FSymbology);
end;

{ TdxBarCodeEAN8Generator }

procedure TdxBarCodeEAN8Generator.AdjustCodeText(var ACodeText: string);
var
  I: Integer;
begin
  inherited;
  for I := Length(ACodeText) to 6 do
    ACodeText := '0' + ACodeText;
  if Length(ACodeText) > 7 then
    ACodeText := Copy(ACodeText, 1, 7);
  ACodeText := ACodeText + GetChecksum(ACodeText);
end;

function TdxBarCodeEAN8Generator.CheckText(const AText: string): TdxBarCodeErrorType;
var
  I: Integer;
begin
  Result := inherited CheckText(AText);
  if Result = bceNone then
    for I := 1 to Length(AText) do
      if not dxCharInSet(AText[I], ['0'..'9']) then
      begin
        Result := bceInvalidCharacters;
        Break;
      end;
end;

function TdxBarCodeEAN8Generator.DefaultBarHeight: Integer;
begin
  Result := Symbology.ModuleWidth * 55;
end;

procedure TdxBarCodeEAN8Generator.DrawText(const ACodeText: string; const ARect: TRect; const ABarSize: TdxSizeF;
  AFont: TFont);
var
  ATextRect: TRect;
  AText: string;
begin
  AText := Copy(GetTextForDrawing(ACodeText), 1, 4);
  ATextRect.Top := Trunc(ARect.Top + ABarSize.cy);
  ATextRect.Bottom := Round(ARect.Bottom - GetBottomOffset * ABarSize.cx);
  ATextRect.Left := Round(ARect.Left + GetLeftOffset * ABarSize.cx + 3 * ABarSize.cx);
  ATextRect.Right := Round(ATextRect.Left + (7 * 4 + 1) * ABarSize.cx);
  FPainter.DrawText(ATextRect, AText, DT_SINGLELINE or DT_CENTER or DT_TOP, AFont);

  AText := Copy(GetTextForDrawing(ACodeText), 5, 4);
  ATextRect.Left := Round(ATextRect.Right + 3 * ABarSize.cx);
  ATextRect.Right := Round(ATextRect.Left + (7 * 4 + 1) * ABarSize.cx);
  FPainter.DrawText(ATextRect, AText, DT_SINGLELINE or DT_CENTER or DT_TOP, AFont);
end;

function TdxBarCodeEAN8Generator.GetScaledFontHeight(const ABounds: TRect; AFont: TFont; const ACodeText: string): Integer;
begin
  Result := -Trunc(Symbology.ModuleWidth * ScaleX(cxRectSize(ABounds), GetInternalSize(ACodeText, AFont)) * 8.3);
  Result := MulDiv(Result, 96, 72);
end;

function TdxBarCodeEAN8Generator.GetTextForDrawing(const ACodeText: string): string;
begin
  Result := GetFullSymbolicCode(ACodeText);
end;

function TdxBarCodeEAN8Generator.GetTextRect(const ABoundsRect, ACodeRect: TRect): TRect;
begin
  Result := ACodeRect;
end;

function TdxBarCodeEAN8Generator.GetBinaryCodeLength(const ACodeText: string): Integer;
begin
  Result := inherited GetBinaryCodeLength(ACodeText) + Length(GetCenterCode);
end;

function TdxBarCodeEAN8Generator.GetFullSymbolicCode(const ACodeText: string): string;
begin
  Result := ACodeText;
  if Length(Result) = 7 then
    Result := Result + GetChecksum(ACodeText);
end;

function TdxBarCodeEAN8Generator.GetLongLinePatternMask(const ACodeText: string): TdxBitArray;
begin
  SetLength(Result, GetBinaryCodeLength(ACodeText));
  Result[0] := bit1;
  Result[2] := bit1;
  Result[32] := bit1;
  Result[34] := bit1;
  Result[64] := bit1;
  Result[66] := bit1;
end;

function TdxBarCodeEAN8Generator.GetLongBarHeight(ABarWidth, ABarHeight: Single): Single;
begin
  Result := ABarHeight + 5 * ABarWidth;
end;

function TdxBarCodeEAN8Generator.GetModuleForSymbolCount: Integer;
begin
  Result := 7;
end;

function TdxBarCodeEAN8Generator.GetStartBinaryCode: string;
begin
  Result := '101';
end;

function TdxBarCodeEAN8Generator.GetStopBinaryCode: string;
begin
  Result := '101';
end;

procedure TdxBarCodeEAN8Generator.PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray;
  ANumberPos: Integer; var AModulePos: Integer);
var
  ANumberMap: string;
  I: Integer;
begin
  if ANumberPos = 5 then
  begin
    for I := AModulePos to AModulePos + Length(GetCenterCode) - 1 do
      ABinaryCode[I] := TdxBit(GetCenterCode[I - AModulePos + 1] = '1');
    AModulePos := AModulePos + Length(GetCenterCode);
  end;

  if ANumberPos < 5 then
    ANumberMap := FNumbersMapEAN[1][StrToInt(AFullSymbolicCode[ANumberPos])]
  else
    ANumberMap := FNumbersMapEAN[3][StrToInt(AFullSymbolicCode[ANumberPos])];

  for I := AModulePos to AModulePos + 6 do
    ABinaryCode[I] := TdxBit(ANumberMap[I - AModulePos + 1] = '1');

  AModulePos := AModulePos + 7;
end;

function TdxBarCodeEAN8Generator.GetLeftOffset: Integer;
begin
  Result := dxBarCodeEAN8LeftQuietZoneSize;
end;

function TdxBarCodeEAN8Generator.GetRightOffset: Integer;
begin
  Result := dxBarCodeEAN8RightQuietZoneSize;
end;

function TdxBarCodeEAN8Generator.GetCenterCode: string;
begin
  Result := '01010';
end;

function TdxBarCodeEAN8Generator.GetChecksum(const ACodeText: string): string;
var
  AChecksum: Integer;
  I: Integer;
begin
  AChecksum := 0;
  for I := 1 to 7 do
    if Odd(I) then
      AChecksum := AChecksum + StrToInt(ACodeText[I]) * 3
    else
      AChecksum := AChecksum + StrToInt(ACodeText[I]);

  AChecksum := (10 - AChecksum mod 10) mod 10;
  Result := IntToStr(AChecksum);
end;

function TdxBarCodeEAN8Generator.GetTextHeight(const AText: string; AFont: TFont): Integer;
var
  AInternalFont: TFont;
begin
  AInternalFont := TFont.Create;
  try
    AInternalFont.Assign(AFont);
    AInternalFont.Height := -MulDiv(Trunc(Symbology.ModuleWidth * 8.3), 96, 72);
    AdjustFontSize('0000', (7 * 4 + 1) * Symbology.ModuleWidth, AInternalFont);
    Result := inherited GetTextHeight(AText, AInternalFont);
  finally
    AInternalFont.Free;
  end;
end;

function TdxBarCodeEAN8Generator.GetSymbology: TdxBarCodeEAN8Symbology;
begin
  Result := TdxBarCodeEAN8Symbology(FSymbology);
end;

{ TdxBarCodeUPCAGenerator }

procedure TdxBarCodeUPCAGenerator.AdjustCodeText(var ACodeText: string);
var
  I: Integer;
begin
  inherited;
  for I := Length(ACodeText) to 10 do
    ACodeText := '0' + ACodeText;
  if Length(ACodeText) > 11 then
    ACodeText := Copy(ACodeText, 1, 11);
  ACodeText := ACodeText + GetChecksum(ACodeText);
end;

function TdxBarCodeUPCAGenerator.CheckText(const AText: string): TdxBarCodeErrorType;
var
  I: Integer;
begin
  Result := inherited CheckText(AText);
  if Result = bceNone then
    for I := 1 to Length(AText) do
      if not dxCharInSet(AText[I], ['0'..'9']) then
      begin
        Result := bceInvalidCharacters;
        Break;
      end;
end;

function TdxBarCodeUPCAGenerator.DefaultBarHeight: Integer;
begin
  Result := Symbology.ModuleWidth * 70;
end;

procedure TdxBarCodeUPCAGenerator.DrawText(const ACodeText: string; const ARect: TRect; const ABarSize: TdxSizeF;
  AFont: TFont);
var
  ATextRect: TRect;
  AText: string;
begin
  AText := Copy(GetTextForDrawing(ACodeText), 1, 1);
  ATextRect.Top := Trunc(ARect.Top + ABarSize.cy);
  ATextRect.Bottom := Round(ARect.Bottom - GetBottomOffset * ABarSize.cx);
  ATextRect.Left := ARect.Left;
  ATextRect.Right := Round(ARect.Left + (GetLeftOffset - 1) * ABarSize.cx);
  FPainter.DrawText(ATextRect, AText, DT_SINGLELINE or DT_LEFT or DT_TOP, AFont);

  AText := Copy(GetTextForDrawing(ACodeText), 2, 5);
  ATextRect.Left := Round(ARect.Left + GetLeftOffset * ABarSize.cx + 10 * ABarSize.cx);
  ATextRect.Right := Round(ATextRect.Left + (7 * 5 + 1) * ABarSize.cx);
  FPainter.DrawText(ATextRect, AText, DT_SINGLELINE or DT_CENTER or DT_TOP, AFont);

  AText := Copy(GetTextForDrawing(ACodeText), 7, 5);
  ATextRect.Left := Round(ATextRect.Right + 3 * ABarSize.cx);
  ATextRect.Right := Round(ATextRect.Left + (7 * 5 + 1) * ABarSize.cx);
  FPainter.DrawText(ATextRect, AText, DT_SINGLELINE or DT_CENTER or DT_TOP, AFont);

  AText := Copy(GetTextForDrawing(ACodeText), 12, 1);
  ATextRect.Left := Round(ATextRect.Right + 11 * ABarSize.cx);
  ATextRect.Right := ARect.Right;
  FPainter.DrawText(ATextRect, AText, DT_SINGLELINE or DT_RIGHT or DT_TOP, AFont);
end;

function TdxBarCodeUPCAGenerator.GetScaledFontHeight(const ABounds: TRect; AFont: TFont; const ACodeText: string): Integer;
begin
  Result := -Trunc(Symbology.ModuleWidth * ScaleX(cxRectSize(ABounds), GetInternalSize(ACodeText, AFont)) * 8.3);
  Result := MulDiv(Result, 96, 72);
end;

function TdxBarCodeUPCAGenerator.GetTextForDrawing(const ACodeText: string): string;
begin
  Result := GetFullSymbolicCode(ACodeText);
end;

function TdxBarCodeUPCAGenerator.GetTextRect(const ABoundsRect, ACodeRect: TRect): TRect;
begin
  Result := ACodeRect;
end;

function TdxBarCodeUPCAGenerator.GetBinaryCodeLength(const ACodeText: string): Integer;
begin
  Result := inherited GetBinaryCodeLength(ACodeText) + Length(GetCenterCode);
end;

function TdxBarCodeUPCAGenerator.GetFullSymbolicCode(const ACodeText: string): string;
begin
  Result := ACodeText;
  if Length(Result) = 11 then
    Result := Result + GetChecksum(ACodeText);
end;

function TdxBarCodeUPCAGenerator.GetLongLinePatternMask(const ACodeText: string): TdxBitArray;
begin
  SetLength(Result, GetBinaryCodeLength(ACodeText));
  Result[0] := bit1;
  Result[2] := bit1;
  Result[3] := bit1;
  Result[4] := bit1;
  Result[5] := bit1;
  Result[6] := bit1;
  Result[7] := bit1;
  Result[8] := bit1;
  Result[9] := bit1;
  Result[46] := bit1;
  Result[48] := bit1;
  Result[85] := bit1;
  Result[86] := bit1;
  Result[87] := bit1;
  Result[88] := bit1;
  Result[89] := bit1;
  Result[90] := bit1;
  Result[91] := bit1;
  Result[92] := bit1;
  Result[94] := bit1;
end;

function TdxBarCodeUPCAGenerator.GetLongBarHeight(ABarWidth, ABarHeight: Single): Single;
begin
  Result := ABarHeight + 5 * ABarWidth;
end;

function TdxBarCodeUPCAGenerator.GetModuleForSymbolCount: Integer;
begin
  Result := 7;
end;

function TdxBarCodeUPCAGenerator.GetStartBinaryCode: string;
begin
  Result := '101';
end;

function TdxBarCodeUPCAGenerator.GetStopBinaryCode: string;
begin
  Result := '101';
end;

procedure TdxBarCodeUPCAGenerator.PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray;
  ANumberPos: Integer; var AModulePos: Integer);
var
  ANumberMap: string;
  I: Integer;
begin
  if ANumberPos = 7 then
  begin
    for I := AModulePos to AModulePos + Length(GetCenterCode) - 1 do
      ABinaryCode[I] := TdxBit(GetCenterCode[I - AModulePos + 1] = '1');
    AModulePos := AModulePos + Length(GetCenterCode);
  end;

  if ANumberPos < 7 then
    ANumberMap := FNumbersMapEAN[1][StrToInt(AFullSymbolicCode[ANumberPos])]
  else
    ANumberMap := FNumbersMapEAN[3][StrToInt(AFullSymbolicCode[ANumberPos])];

  for I := AModulePos to AModulePos + 6 do
    ABinaryCode[I] := TdxBit(ANumberMap[I - AModulePos + 1] = '1');

  AModulePos := AModulePos + 7;
end;

function TdxBarCodeUPCAGenerator.GetLeftOffset: Integer;
begin
  Result := dxBarCodeUPCALeftQuietZoneSize;
end;

function TdxBarCodeUPCAGenerator.GetRightOffset: Integer;
begin
  Result := dxBarCodeUPCARightQuietZoneSize;
end;

function TdxBarCodeUPCAGenerator.GetCenterCode: string;
begin
  Result := '01010';
end;

function TdxBarCodeUPCAGenerator.GetChecksum(const ACodeText: string): string;
var
  AChecksum: Integer;
  I: Integer;
begin
  AChecksum := 0;
  for I := 1 to 11 do
    if Odd(I) then
      AChecksum := AChecksum + StrToInt(ACodeText[I]) * 3
    else
      AChecksum := AChecksum + StrToInt(ACodeText[I]);

  AChecksum := (10 - AChecksum mod 10) mod 10;
  Result := IntToStr(AChecksum);
end;

function TdxBarCodeUPCAGenerator.GetTextHeight(const AText: string; AFont: TFont): Integer;
var
  AInternalFont: TFont;
begin
  AInternalFont := TFont.Create;
  try
    AInternalFont.Assign(AFont);
    AInternalFont.Height := -MulDiv(Trunc(Symbology.ModuleWidth * 8.3), 96, 72);
    AdjustFontSize('0000', (7 * 5 + 1) * Symbology.ModuleWidth, AInternalFont);
    Result := inherited GetTextHeight(AText, AInternalFont);
  finally
    AInternalFont.Free;
  end;
end;

function TdxBarCodeUPCAGenerator.GetSymbology: TdxBarCodeUPCASymbology;
begin
  Result := TdxBarCodeUPCASymbology(FSymbology);
end;

{ TdxBarCodeUPCEGenerator }

procedure TdxBarCodeUPCEGenerator.AdjustCodeText(var ACodeText: string);
var
  I: Integer;
begin
  inherited;
  for I := Length(ACodeText) to 6 do
    ACodeText := '0' + ACodeText;
  if Length(ACodeText) > 7 then
    ACodeText := Copy(ACodeText, 1, 7);
end;

function TdxBarCodeUPCEGenerator.CheckText(const AText: string): TdxBarCodeErrorType;
var
  I: Integer;
begin
  Result := inherited CheckText(AText);
  if (Result = bceNone) and (Length(AText) > 0) then
  begin
    for I := 1 to Length(AText) do
      if not dxCharInSet(AText[I], ['0'..'9']) then
      begin
        Result := bceInvalidCharacters;
        Break;
      end;
    if (Result = bceNone) and not (StrToInt(AText[1]) in [0, 1]) then
      Result := bceInvalidCharacters;
  end;
end;

function TdxBarCodeUPCEGenerator.DefaultBarHeight: Integer;
begin
  Result := Symbology.ModuleWidth * 70;
end;

procedure TdxBarCodeUPCEGenerator.DrawText(const ACodeText: string; const ARect: TRect; const ABarSize: TdxSizeF;
  AFont: TFont);
var
  ATextRect: TRect;
  AText: string;
begin
  AText := Copy(GetTextForDrawing(ACodeText), 1, 1);
  ATextRect.Top := Trunc(ARect.Top + ABarSize.cy + GetBottomOffset * ABarSize.cx);
  ATextRect.Bottom := Round(ARect.Bottom - GetBottomOffset * ABarSize.cx);
  ATextRect.Left := ARect.Left;
  ATextRect.Right := Round(ARect.Left + (GetLeftOffset - 1) * ABarSize.cx);
  FPainter.DrawText(ATextRect, AText, DT_SINGLELINE or DT_LEFT or DT_TOP, AFont);

  AText := Copy(GetTextForDrawing(ACodeText), 2, 6);
  ATextRect.Left := Round(ARect.Left + GetLeftOffset * ABarSize.cx + 3 * ABarSize.cx);
  ATextRect.Right := Round(ATextRect.Left + (7 * 6 + 1) * ABarSize.cx);
  FPainter.DrawText(ATextRect, AText, DT_SINGLELINE or DT_CENTER or DT_TOP, AFont);

  AText := Copy(GetTextForDrawing(ACodeText), 8, 1);
  ATextRect.Left := Round(ATextRect.Right + 6 * ABarSize.cx);
  ATextRect.Right := ARect.Right;
  FPainter.DrawText(ATextRect, AText, DT_SINGLELINE or DT_RIGHT or DT_TOP, AFont);
end;

function TdxBarCodeUPCEGenerator.GetScaledFontHeight(const ABounds: TRect; AFont: TFont; const ACodeText: string): Integer;
begin
  Result := -Trunc(Symbology.ModuleWidth * ScaleX(cxRectSize(ABounds), GetInternalSize(ACodeText, AFont)) * 8.3);
  Result := MulDiv(Result, 96, 72);
end;

function TdxBarCodeUPCEGenerator.GetTextForDrawing(const ACodeText: string): string;
begin
  Result := GetFullSymbolicCode(ACodeText);
end;

function TdxBarCodeUPCEGenerator.GetTextRect(const ABoundsRect, ACodeRect: TRect): TRect;
begin
  Result := ACodeRect;
end;

function TdxBarCodeUPCEGenerator.GetBinaryCodeLength(const ACodeText: string): Integer;
begin
  Result := inherited GetBinaryCodeLength(ACodeText) - 2 * GetModuleForSymbolCount;
end;

function TdxBarCodeUPCEGenerator.GetFullSymbolicCode(const ACodeText: string): string;
begin
  Result := ACodeText;
  if Length(Result) = 7 then
    Result := Result + GetChecksum(ACodeText);
end;

function TdxBarCodeUPCEGenerator.GetLongLinePatternMask(const ACodeText: string): TdxBitArray;
begin
  SetLength(Result, GetBinaryCodeLength(ACodeText));
  Result[0] := bit1;
  Result[2] := bit1;
  Result[46] := bit1;
  Result[48] := bit1;
  Result[50] := bit1;
end;

function TdxBarCodeUPCEGenerator.GetLongBarHeight(ABarWidth, ABarHeight: Single): Single;
begin
  Result := ABarHeight + 5 * ABarWidth;
end;

function TdxBarCodeUPCEGenerator.GetModuleForSymbolCount: Integer;
begin
  Result := 7;
end;

function TdxBarCodeUPCEGenerator.GetStartBinaryCode: string;
begin
  Result := '101';
end;

function TdxBarCodeUPCEGenerator.GetStopBinaryCode: string;
begin
  Result := '010101';
end;

procedure TdxBarCodeUPCEGenerator.PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray;
  ANumberPos: Integer; var AModulePos: Integer);
var
  ANumberMap: string;
  I: Integer;
  ANumberMapIndex: Integer;
begin
  if (ANumberPos > 1) and (ANumberPos < 8) then
  begin
    ANumberMapIndex := StrToInt(FLastNumberMapUPCE[
      StrToInt(AFullSymbolicCode[1])][StrToInt(AFullSymbolicCode[8])][ANumberPos - 1]);
    ANumberMap := FNumbersMapEAN[ANumberMapIndex][StrToInt(AFullSymbolicCode[ANumberPos])];
    for I := AModulePos to AModulePos + 6 do
      ABinaryCode[I] := TdxBit(ANumberMap[I - AModulePos + 1] = '1');

    AModulePos := AModulePos + 7;
  end;
end;

function TdxBarCodeUPCEGenerator.GetLeftOffset: Integer;
begin
  Result := dxBarCodeUPCELeftQuietZoneSize;
end;

function TdxBarCodeUPCEGenerator.GetRightOffset: Integer;
begin
  Result := dxBarCodeUPCERightQuietZoneSize;
end;

function TdxBarCodeUPCEGenerator.GetChecksum(const ACodeText: string): string;
var
  AChecksum: Integer;
  I: Integer;
begin
  AChecksum := 0;
  for I := 1 to 7 do
    if Odd(I) then
      AChecksum := AChecksum + StrToInt(ACodeText[I]) * 3
    else
      AChecksum := AChecksum + StrToInt(ACodeText[I]);

  AChecksum := (10 - AChecksum mod 10) mod 10;
  Result := IntToStr(AChecksum);
end;

function TdxBarCodeUPCEGenerator.GetTextHeight(const AText: string; AFont: TFont): Integer;
var
  AInternalFont: TFont;
begin
  AInternalFont := TFont.Create;
  try
    AInternalFont.Assign(AFont);
    AInternalFont.Height := -MulDiv(Trunc(Symbology.ModuleWidth * 8.3), 96, 72);
    AdjustFontSize('0000', (7 * 6 + 1) * Symbology.ModuleWidth, AInternalFont);
    Result := inherited GetTextHeight(AText, AInternalFont);
  finally
    AInternalFont.Free;
  end;
end;

function TdxBarCodeUPCEGenerator.GetSymbology: TdxBarCodeUPCESymbology;
begin
  Result := TdxBarCodeUPCESymbology(FSymbology);
end;

{ TdxBarCode128Generator }

function TdxBarCode128Generator.GetFullSymbolicCode(const ACodeText: string): String;

  function NeedSelectTypeA(APos: Integer; APrevCodeType: TdxBarCode128CharacterSet): Boolean;
  var
    I: Integer;
  begin
    I := APos;
    Result := False;
    while (I <= Length(ACodeText)) and (dxCharInSet(ACodeText[I], ['0'..'9']) or (Ord(ACodeText[I]) < 32)) do
    begin
      Result := Result or (Ord(ACodeText[I]) < 32);
      Inc(I);
    end;
    Result := Result or ((APrevCodeType = csA) and dxCharInSet(ACodeText[APos], ['0'..'9']));
  end;

  function NeedSelectTypeC(APos: Integer; APrevCodeType: TdxBarCode128CharacterSet): Boolean;
  var
    I: Integer;
  begin
    I := APos;
    while (I <= Length(ACodeText)) and dxCharInSet(ACodeText[I], ['0'..'9']) do
      Inc(I);

    Result := (I - APos > 3) or ((I - APos > 1) and (APrevCodeType = csC));
  end;

  function GetCodeType(ACharacterSet: TdxBarCode128CharacterSet; APrevCodeType: TdxBarCode128CharacterSet;
    var APos: Integer; out ASymbol: Char): TdxBarCode128CharacterSet;
  begin
    if ACharacterSet = csAuto then
    begin
      if NeedSelectTypeC(APos, APrevCodeType) then
        Result := csC
      else
        if NeedSelectTypeA(APos, APrevCodeType) then
          Result := csA
        else
          Result := csB;
    end
    else
      Result := APrevCodeType;

    if Result = csC then
    begin
      ASymbol := Char(StrToInt(Copy(ACodeText, APos, 2)) + 32);
      Inc(APos, 2);
    end
    else
    begin
      ASymbol := ACodeText[APos];
      Inc(APos, 1);
    end;
  end;

var
  ASymbolPos: Integer;
  ACodeType: TdxBarCode128CharacterSet;
  APrevCodeType: TdxBarCode128CharacterSet;
  ASymbol: Char;
begin
  Result := '';
  ASymbolPos := 1;
  if Symbology.CharacterSet = csAuto then
    ACodeType := csB
  else
    ACodeType := Symbology.CharacterSet;

  while ASymbolPos <= Length(ACodeText) do
  begin
    APrevCodeType := ACodeType;
    ACodeType := GetCodeType(Symbology.CharacterSet, ACodeType, ASymbolPos, ASymbol);
    if Result = '' then
    begin
      case ACodeType of
        csA: Result := Result + Char(135);
        csB: Result := Result + Char(136);
        csC: Result := Result + Char(137);
      end;
      if IsEAN then
        Result := Result + Char(134);
    end
    else
      if ACodeType <> APrevCodeType then
        case APrevCodeType of
          csA:
            case ACodeType of
              csB: Result := Result + Char(132);
              csC: Result := Result + Char(131);
            end;
          csB:
            case ACodeType of
              csA: Result := Result + Char(133);
              csC: Result := Result + Char(131);
            end;
          csC:
            case ACodeType of
              csA: Result := Result + Char(133);
              csB: Result := Result + Char(132);
            end;
        end;
    Result := Result + ASymbol;
  end;
  Result := Result + Char(StrToInt(GetChecksum(Result)) + 32);
  Result := Result + Char(138);
end;

function TdxBarCode128Generator.GetInternalSize(const ACodeText: string; const AFont: TFont): TSize;
begin
  Result := inherited GetInternalSize(ACodeText, AFont);
  Result.cx := Max(Result.cx, cxTextWidth(AFont, GetTextForDrawing(ACodeText)));
end;

function TdxBarCode128Generator.CheckText(const AText: string): TdxBarCodeErrorType;
begin
  Result := CheckCodeByCharset(AText, Symbology.CharacterSet);
end;

function TdxBarCode128Generator.GetModuleForSymbolCount: Integer;
begin
  Result := 11;
end;

function TdxBarCode128Generator.GetStopBinaryCode: string;
begin
  Result := '11';
end;

procedure TdxBarCode128Generator.PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray;
  ANumberPos: Integer; var AModulePos: Integer);
var
  I: Integer;
  ANumberMap: string;
begin
  ANumberMap := FSymbolMap128[GetIndexBySymbol(AFullSymbolicCode[ANumberPos])][3];
  for I := AModulePos to AModulePos + 10 do
    ABinaryCode[I] := TdxBit(ANumberMap[I - AModulePos + 1] = '1');

  AModulePos := AModulePos + 11;
end;

function TdxBarCode128Generator.GetLeftOffset: Integer;
begin
  Result := dxBarCode128LeftQuietZoneSize;
end;

function TdxBarCode128Generator.GetRightOffset: Integer;
begin
  Result := dxBarCode128RightQuietZoneSize;
end;

function TdxBarCode128Generator.GetChecksum(const ACodeText: string): string;
var
  AChecksum: LongInt;
  I: Integer;
begin
  AChecksum := 0;
  if Length(ACodeText) > 0 then
  begin
    AChecksum := GetIndexBySymbol(ACodeText[1]);
    for I := 2 to Length(ACodeText) do
      AChecksum := AChecksum + GetIndexBySymbol(ACodeText[I]) * (I - 1);
  end;

  AChecksum := AChecksum mod 103;
  Result := IntToStr(AChecksum);
end;

function TdxBarCode128Generator.IsEAN: Boolean;
begin
  Result := False;
end;

function TdxBarCode128Generator.CheckCodeByCharset(const ACodeText: string;
  ACharset: TdxBarCode128CharacterSet): TdxBarCodeErrorType;

  function IsInvalidChar(APos: Integer): Boolean;
  begin
    case ACharset of
      csA:
        Result := Ord(ACodeText[APos]) > 95;
      csB:
        Result := (Ord(ACodeText[APos]) < 32) or (Ord(ACodeText[APos]) > 126);
      csC:
        Result := not dxCharInSet(ACodeText[APos], ['0'..'9']);
    else // csAuto
      Result := Ord(ACodeText[APos]) > 126;
    end;
  end;

var
  I: Integer;
begin
  Result := bceNone;
  if (ACharset = csC) and Odd(Length(ACodeText)) then
    Result := bceInvalidTextFormat
  else
    for I := 1 to Length(ACodeText) do
      if IsInvalidChar(I) then
      begin
        Result := bceInvalidTextFormat;
        Break;
      end;
end;

function TdxBarCode128Generator.GetIndexBySymbol(ASymbol: Char): Integer;
begin
  if Ord(ASymbol) < 32 then
    Result := Ord(ASymbol) + 64
  else
    Result := Ord(ASymbol) - 32;
end;

function TdxBarCode128Generator.GetSymbology: TdxBarCode128Symbology;
begin
  Result := TdxBarCode128Symbology(FSymbology);
end;

{ TdxBarCodeITFTypeGenerator }

procedure TdxBarCodeITFTypeGenerator.DrawBars(const ABoundsRect: TRect; const ACodeText: string; const ABarSize: TdxSizeF);
var
  I: Integer;
  ACurrentPosX: Single;
  ALineRect: TdxRectF;
  ABinaryCode: TdxBitArray;
begin
  ALineRect.Left := ABoundsRect.Left + GetLeftOffset * ABarSize.cx;
  ALineRect.Top := ABoundsRect.Top + GetTopOffset * ABarSize.cx;
  ALineRect.Right := ABoundsRect.Left + GetLeftOffset * ABarSize.cx;
  ALineRect.Bottom := ABoundsRect.Top + GetTopOffset * ABarSize.cx + ABarSize.cy;
  ACurrentPosX := ALineRect.Left;
  ABinaryCode := GetBinaryCode(ACodeText);
  for I := 0 to Length(ABinaryCode) - 1 do
  begin
    if ABinaryCode[I] = bit0 then
      ACurrentPosX := ACurrentPosX + ABarSize.cx
    else
      ACurrentPosX := ACurrentPosX + Symbology.WideNarrowRatio * ABarSize.cx;

    ALineRect.Left := ALineRect.Right;
    ALineRect.Right := ACurrentPosX;
    if not Odd(I) then
      FPainter.FillRect(Rect(Round(ALineRect.Left), Round(ALineRect.Top), Round(ALineRect.Right), Round(ALineRect.Bottom)),
        Symbology.ModuleColor);
  end;
end;

function TdxBarCodeITFTypeGenerator.GetCodeWidth(const ACodeText: string): Integer;
var
  ABinaryCode: TdxBitArray;
  ACodeWidth: Single;
  I: Integer;
begin
  ABinaryCode := GetBinaryCode(ACodeText);

  ACodeWidth := (GetLeftOffset + GetRightOffset) * Symbology.ModuleWidth;
  for I := 0 to GetBinaryCodeLength(ACodeText) - 1 do
    if ABinaryCode[I] = bit1 then
      ACodeWidth := ACodeWidth + Symbology.ModuleWidth * Symbology.WideNarrowRatio
    else
      ACodeWidth := ACodeWidth + Symbology.ModuleWidth;

  Result := Trunc(ACodeWidth) + 1;
end;

function TdxBarCodeITFTypeGenerator.GetInternalSize(const ACodeText: string; const AFont: TFont): TSize;
begin
  Result.cx := GetCodeWidth(ACodeText);
  if Symbology.ShowText then
    Result.cy := DefaultBarHeight + cxTextHeight(AFont)
  else
    Result.cy := DefaultBarHeight;

  Result.cy := Result.cy + (GetTopOffset + GetBottomOffset) * Symbology.ModuleWidth;
end;

function TdxBarCodeITFTypeGenerator.GetSymbology: TdxBarCodeITFTypeSymbology;
begin
  Result := TdxBarCodeITFTypeSymbology(FSymbology);
end;

{ TdxBarCodeInterleaved2Of5Generator }

function TdxBarCodeInterleaved2Of5Generator.CheckText(const AText: string): TdxBarCodeErrorType;
var
  I: Integer;
begin
  Result := inherited CheckText(AText);
  for I := 1 to Length(AText) do
    if not dxCharInSet(AText[I], ['0'..'9']) then
    begin
      Result := bceInvalidCharacters;
      Break;
    end;
end;

function TdxBarCodeInterleaved2Of5Generator.GetTextForDrawing(const ACodeText: string): string;
begin
  Result := GetFullSymbolicCode(ACodeText);
end;

function TdxBarCodeInterleaved2Of5Generator.GetFullSymbolicCode(const ACodeText: string): string;
begin
  Result := ACodeText;
  if Symbology.Checksum then
    Result := Result + GetChecksum(Result);
  if Odd(Length(Result)) then
    Result := '0' + Result;
end;

function TdxBarCodeInterleaved2Of5Generator.GetModuleForSymbolCount: Integer;
begin
  Result := 5;
end;

function TdxBarCodeInterleaved2Of5Generator.GetStartBinaryCode: string;
begin
  Result := '0000';
end;

function TdxBarCodeInterleaved2Of5Generator.GetStopBinaryCode: string;
begin
  Result := '100';
end;

procedure TdxBarCodeInterleaved2Of5Generator.PopulatePartBinaryCode(const AFullSymbolicCode: string;
  ABinaryCode: TdxBitArray; ANumberPos: Integer; var AModulePos: Integer);
var
  I: Integer;
  ANumberMap1, ANumberMap2: string;
begin
  if Odd(ANumberPos) then
  begin
    ANumberMap1 := FSymbolMap_2_5[0, StrToInt(AFullSymbolicCode[ANumberPos])];
    ANumberMap2 := FSymbolMap_2_5[1, StrToInt(AFullSymbolicCode[ANumberPos + 1])];
    for I := 1 to 10 do
    begin
      ABinaryCode[AModulePos] := TdxBit((ANumberMap1[I] = '1') or (ANumberMap2[I] = '1'));
      Inc(AModulePos);
    end;
  end;
end;

function TdxBarCodeInterleaved2Of5Generator.GetLeftOffset: Integer;
begin
  Result := dxBarCodeInterleaved2Of5LeftQuietZoneSize;
end;

function TdxBarCodeInterleaved2Of5Generator.GetRightOffset: Integer;
begin
  Result := dxBarCodeInterleaved2Of5RightQuietZoneSize;
end;

function TdxBarCodeInterleaved2Of5Generator.GetChecksum(const ACodeText: string): string;
var
  I: Integer;
  AEvenSum, AOddSum, ACount: Integer;
begin
  ACount := Length(ACodeText);
  AEvenSum := 0;
  AOddSum := 0;
  for I := 1 to ACount do
  begin
    if Odd(I - 1) then
      AOddSum := AOddSum + StrToInt(ACodeText[I])
    else
      AEvenSum := AEvenSum + StrToInt(ACodeText[I]);
  end;
  if Odd(ACount) then
    AEvenSum := AEvenSum * 3 + AOddSum
  else
    AEvenSum := AEvenSum + AOddSum * 3;

  AEvenSum := AEvenSum mod 10;
  if AEvenSum <> 0 then
    AEvenSum := 10 - AEvenSum;

  Result := IntToStr(AEvenSum);
end;

function TdxBarCodeInterleaved2Of5Generator.GetSymbology: TdxBarCodeInterleaved2Of5Symbology;
begin
  Result := TdxBarCodeInterleaved2Of5Symbology(FSymbology);
end;

{ TdxBarCode39Generator }

function TdxBarCode39Generator.GetFullSymbolicCode(const ACodeText: string): string;
begin
  Result := ACodeText;
  if Symbology.Checksum then
    Result := Result + GetChecksum(Result);
end;

function TdxBarCode39Generator.CheckText(const AText: string): TdxBarCodeErrorType;
var
  I: Integer;
begin
  Result := inherited CheckText(AText);
  for I := 1 to Length(AText) do
    if not CharInSet(AText[I], [' ', '$', '%', '+', '-'..'9', 'A'..'Z']) then
    begin
      Result := bceInvalidCharacters;
      Break;
    end;
end;

function TdxBarCode39Generator.GetTextForDrawing(const ACodeText: string): string;
begin
  Result := GetFullSymbolicCode(ACodeText);
end;

function TdxBarCode39Generator.GetModuleForSymbolCount: Integer;
begin
  Result := 10;
end;

function TdxBarCode39Generator.GetStartBinaryCode: string;
begin
  Result := GetEdgeCode + '0';
end;

function TdxBarCode39Generator.GetStopBinaryCode: string;
begin
  Result := GetEdgeCode;
end;

procedure TdxBarCode39Generator.PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray;
  ANumberPos: Integer; var AModulePos: Integer);
var
  I: Integer;
  ANumberMap: string;
begin
  ANumberMap := FSymbolMapCode39[GetCharMapIndex(AFullSymbolicCode, ANumberPos)][1];
  for I := 1 to 9 do
  begin
    ABinaryCode[AModulePos] := TdxBit(ANumberMap[I] = '1');
    Inc(AModulePos);
  end;
  Inc(AModulePos);
end;

function TdxBarCode39Generator.GetLeftOffset: Integer;
begin
  Result := dxBarCode39LeftQuietZoneSize;
end;

function TdxBarCode39Generator.GetRightOffset: Integer;
begin
  Result := dxBarCode39RightQuietZoneSize;
end;

function TdxBarCode39Generator.GetChecksum(const ACodeText: string): string;
var
  I: Integer;
  ASum: Integer;
begin
  ASum := 0;
  for I := 1 to Length(ACodeText) do
    ASum := ASum + GetCharMapIndex(ACodeText, I);
  Result := FSymbolMapCode39[ASum mod 43][0];
end;

function TdxBarCode39Generator.GetCharMapIndex(const AFullSymbolicCode: string; ACharIndex: Integer): Integer;
begin
  Result := Ord(AFullSymbolicCode[ACharIndex]);
  case Result of
    48..57: Result := Result - 48;
    65..90: Result := Result - 55;
    45: Result := 36;
    46: Result := 37;
    32: Result := 38;
    36: Result := 39;
    47: Result := 40;
    43: Result := 41;
    37: Result := 42;
  end;
end;

function TdxBarCode39Generator.GetEdgeCode: string;
begin
  Result := '010010100'
end;

function TdxBarCode39Generator.GetSymbology: TdxBarCode39Symbology;
begin
  Result := TdxBarCode39Symbology(FSymbology);
end;

{ TdxBarCode39ExtendedGenerator }

function TdxBarCode39ExtendedGenerator.CheckText(const AText: string): TdxBarCodeErrorType;
var
  I: Integer;
begin
  Result := bceNone;
  for I := 1 to Length(AText) do
  begin
    if not (Ord(AText[I]) in [0..127]) then
    begin
      Result := bceInvalidCharacters;
      Break;
    end;
  end;
end;

function TdxBarCode39ExtendedGenerator.GetTextForDrawing(const ACodeText: string): string;
var
  I: Integer;
begin
  Result := '';
  if Symbology.Checksum then
  begin
    for I := 1 to Length(ACodeText) do
      Result := Result + FASCIIToCode39Symbols[Ord(ACodeText[I])];
    Result := ACodeText + GetChecksum(Result);
  end
  else
    Result := ACodeText;
end;

function TdxBarCode39ExtendedGenerator.GetFullSymbolicCode(const ACodeText: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(ACodeText) do
    Result := Result + FASCIIToCode39Symbols[Ord(ACodeText[I])];
  if Symbology.Checksum then
    Result := Result + GetChecksum(Result);
end;

function TdxBarCode39ExtendedGenerator.GetSymbology: TdxBarCode39ExtendedSymbology;
begin
  Result := TdxBarCode39ExtendedSymbology(FSymbology);
end;

{ TdxBarCode93Generator }

function TdxBarCode93Generator.CheckText(const AText: string): TdxBarCodeErrorType;
var
  I: Integer;
begin
  Result := inherited CheckText(AText);
  for I := 1 to Length(AText) do
  begin
    if not CharInSet(AText[I], [' ', '$', '%', '+', '-'..'9', 'A'..'Z']) then
    begin
      Result := bceInvalidCharacters;
      Break;
    end;
  end;
end;

function TdxBarCode93Generator.GetFullSymbolicCode(const ACodeText: string): string;
begin
  Result := ACodeText + GetChecksum(ACodeText, 20);
  Result := Result + GetChecksum(Result, 15);
end;

function TdxBarCode93Generator.GetInternalSize(const ACodeText: string; const AFont: TFont): TSize;
begin
  Result := inherited GetInternalSize(ACodeText, AFont);
  Result.cx := Max(Result.cx, cxTextWidth(AFont, GetTextForDrawing(ACodeText)));
end;

function TdxBarCode93Generator.GetModuleForSymbolCount: Integer;
begin
  Result := 9;
end;

procedure TdxBarCode93Generator.PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray;
  ANumberPos: Integer; var AModulePos: Integer);
var
  I: Integer;
  ANumberMap: string;
begin
  ANumberMap := FSymbolMapCode93[GetSymbolNumber(AFullSymbolicCode[ANumberPos])][1];
  for I := 1 to 9 do
    ABinaryCode[AModulePos + I - 1] := TdxBit((ANumberMap[I]) = '1');

  AModulePos := AModulePos + 9;
end;

function TdxBarCode93Generator.GetStartBinaryCode: string;
begin
  Result := '101011110';
end;

function TdxBarCode93Generator.GetStopBinaryCode: string;
begin
  Result := '1010111101';
end;

function TdxBarCode93Generator.GetLeftOffset: Integer;
begin
  Result := dxBarCode93LeftQuietZoneSize;
end;

function TdxBarCode93Generator.GetRightOffset: Integer;
begin
  Result := dxBarCode93RightQuietZoneSize;
end;

function TdxBarCode93Generator.GetChecksum(const ACodeText: string; AWeighting: Integer): string;
var
  I: Integer;
  AChecksum: Integer;
  AWeight: Integer;
begin
  AChecksum := 0;
  for I := 1 to Length(ACodeText) do
  begin
    AWeight := Length(ACodeText) - I;
    AWeight := AWeight mod AWeighting + 1;

    AChecksum := AChecksum + AWeight * GetSymbolNumber(ACodeText[I]);
  end;
  AChecksum := AChecksum mod 47;
  Result := FSymbolMapCode93[AChecksum][0];
end;

function TdxBarCode93Generator.GetSymbolNumber(ASymbol: Char): Integer;
begin
  Result := Ord(ASymbol);
  case Result of
    48..57: Result := Result - 48;
    65..90: Result := Result - 55;
    45: Result := 36;
    46: Result := 37;
    32: Result := 38;
    36: Result := 39;
    47: Result := 40;
    43: Result := 41;
    37: Result := 42;
    58..61: Result := Result - 15;
  end;
end;

function TdxBarCode93Generator.GetSymbology: TdxBarCode93Symbology;
begin
  Result := TdxBarCode93Symbology(FSymbology);
end;

{ TdxBarCode93ExtendedGenerator }

function TdxBarCode93ExtendedGenerator.CheckText(const AText: string): TdxBarCodeErrorType;
var
  I: Integer;
begin
  Result := bceNone;
  for I := 1 to Length(AText) do
  begin
    if not (Ord(AText[I]) in [0..127]) then
    begin
      Result := bceInvalidCharacters;
      Break;
    end;
  end;
end;

function TdxBarCode93ExtendedGenerator.GetFullSymbolicCode(const ACodeText: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(ACodeText) do
    Result := Result + FASCIIToCode93Symbols[Ord(ACodeText[I])];

  Result := inherited GetFullSymbolicCode(Result);
end;

function TdxBarCode93ExtendedGenerator.GetSymbology: TdxBarCode93ExtendedSymbology;
begin
  Result := TdxBarCode93ExtendedSymbology(FSymbology);
end;

{ TdxBarCodeMSIGenerator }

function TdxBarCodeMSIGenerator.CheckText(const AText: string): TdxBarCodeErrorType;
var
  I: Integer;
begin
  Result := inherited CheckText(AText);
  for I := 1 to Length(AText) do
  begin
    if not dxCharInSet(AText[I], ['0'..'9']) then
    begin
      Result := bceInvalidCharacters;
      Break;
    end;
  end;
end;

function TdxBarCodeMSIGenerator.GetTextForDrawing(const ACodeText: string): string;
begin
  Result := GetFullSymbolicCode(ACodeText);
end;

function TdxBarCodeMSIGenerator.GetFullSymbolicCode(const ACodeText: string): string;
begin
  Result := ACodeText + GetChecksum(ACodeText);
end;

function TdxBarCodeMSIGenerator.GetInternalSize(const ACodeText: string; const AFont: TFont): TSize;
begin
  Result := inherited GetInternalSize(ACodeText, AFont);
  Result.cx := Max(Result.cx, cxTextWidth(AFont, GetTextForDrawing(ACodeText)));
end;

function TdxBarCodeMSIGenerator.GetModuleForSymbolCount: Integer;
begin
  Result := 12;
end;

function TdxBarCodeMSIGenerator.GetStartBinaryCode: string;
begin
  Result := '110';
end;

function TdxBarCodeMSIGenerator.GetStopBinaryCode: string;
begin
  Result := '1001';
end;

procedure TdxBarCodeMSIGenerator.PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray;
  ANumberPos: Integer; var AModulePos: Integer);
var
  ANumberMap: string;
  I: Integer;
begin
  ANumberMap := FNumberMapMSI[StrToInt(AFullSymbolicCode[ANumberPos])];
  for I := AModulePos to AModulePos + 11 do
    ABinaryCode[I] := TdxBit(ANumberMap[I - AModulePos + 1] = '1');

  AModulePos := AModulePos + 12;
end;

function TdxBarCodeMSIGenerator.GetLeftOffset: Integer;
begin
  Result := dxBarCodeMSILeftQuietZoneSize;
end;

function TdxBarCodeMSIGenerator.GetRightOffset: Integer;
begin
  Result := dxBarCodeMSIRightQuietZoneSize;
end;

function TdxBarCodeMSIGenerator.GetChecksum(const ACodeText: string): string;
var
  I: Integer;
  AChecksum: Integer;
  AValue: Integer;
begin
  AChecksum := 0;
  for I := 1 to Length(ACodeText) do
  begin
    AValue := StrToInt(ACodeText[Length(ACodeText) - I + 1]);
    if Odd(I) then
    begin
      AValue := 2 * AValue;
      if AValue > 9 then
        AValue := AValue - 9;
    end;
    AChecksum := AChecksum + AValue;
  end;
  AChecksum := (10 - (AChecksum mod 10)) mod 10;
  Result := IntToStr(AChecksum);
end;

function TdxBarCodeMSIGenerator.GetSymbology: TdxBarCodeMSISymbology;
begin
  Result := TdxBarCodeMSISymbology(FSymbology);
end;

{ TdxBarCode11Generator }

function TdxBarCode11Generator.CheckText(const AText: string): TdxBarCodeErrorType;
var
  I: Integer;
begin
  Result := inherited CheckText(AText);
  for I := 1 to Length(AText) do
  begin
    if not CharInSet(AText[I], ['-', '0'..'9']) then
    begin
      Result := bceInvalidCharacters;
      Break;
    end;
  end;
end;

function TdxBarCode11Generator.GetFullSymbolicCode(const ACodeText: string): string;
begin
  Result := ACodeText + GetChecksum(ACodeText);
end;

function TdxBarCode11Generator.GetModuleForSymbolCount: Integer;
begin
  Result := 6;
end;

function TdxBarCode11Generator.GetStartBinaryCode: string;
begin
  Result := '001100';
end;

function TdxBarCode11Generator.GetStopBinaryCode: string;
begin
  Result := '001100';
end;

procedure TdxBarCode11Generator.PopulatePartBinaryCode(const AFullSymbolicCode: string; ABinaryCode: TdxBitArray;
  ANumberPos: Integer; var AModulePos: Integer);
var
  I: Integer;
  ASymbolMap: string;
begin
  ASymbolMap := FSymbolMapCode11[GetSymbolNumber(AFullSymbolicCode[ANumberPos])];
  for I := 1 to 6 do
  begin
    ABinaryCode[AModulePos] := TdxBit(ASymbolMap[I] = '1');
    Inc(AModulePos);
  end;
end;

function TdxBarCode11Generator.GetLeftOffset: Integer;
begin
  Result := dxBarCode11LeftQuietZoneSize;
end;

function TdxBarCode11Generator.GetRightOffset: Integer;
begin
  Result := dxBarCode11RightQuietZoneSize;
end;

function TdxBarCode11Generator.GetChecksum(const ACodeText: string): string;

  function GetInternalChecksum(const AChecksumStr: string; AWeighting: Integer): string;
  var
    I: Integer;
    AChecksum: Integer;
    AWeight: Integer;
    ASymbols: string;
  begin
    AChecksum := 0;
    ASymbols := ACodeText + AChecksumStr;
    for I := 1 to Length(ASymbols) do
    begin
      AWeight := Length(ASymbols) - I;
      AWeight := AWeight mod AWeighting + 1;

      AChecksum := AChecksum + AWeight * GetSymbolNumber(ASymbols[I]);
    end;
    AChecksum := AChecksum mod 11;
    if AChecksum = 10 then
      AChecksum := Ord('-')
    else
      AChecksum := AChecksum + Ord('0');

    Result := AChecksumStr + Char(AChecksum);
  end;

begin
  if Length(ACodeText) < 10 then
    Result := GetInternalChecksum('', 10)
  else
    Result := GetInternalChecksum(GetInternalChecksum('', 10), 9);
end;

function TdxBarCode11Generator.GetSymbolNumber(ASymbol: Char): Integer;
begin
  if ASymbol = '-' then
    Result := 10
  else
    Result := Ord(ASymbol) - Ord('0');
end;

function TdxBarCode11Generator.GetSymbology: TdxBarCode11Symbology;
begin
  Result := TdxBarCode11Symbology(FSymbology);
end;

{ TdxBarCodeQRCodeGenerator }

function TdxBarCodeQRCodeGenerator.CalculateRect(const ACodeText: string; AFont: TFont;
  var ARect: TRect): TdxBarCodeErrorType;

  function GetRectSize(const AText: string): TSize;
  begin
    Result := GetInternalSize(AText, AFont);
    Result.cx := Round(Result.cx * ScaleX(cxRectSize(ARect), GetInternalSize(AText, AFont)));
    Result.cy := Round(Result.cy * ScaleY(cxRectSize(ARect), GetInternalSize(AText, AFont)));
  end;

var
  ABarCodeSize: TSize;
begin
  Result := bceNone;
  ABarCodeSize := GetRectSize('');
  if (ABarCodeSize.cx > cxRectWidth(ARect)) or (ABarCodeSize.cy > cxRectHeight(ARect)) then
    Result := bceBoundsTooSmall;
  ARect := cxRectCenter(ARect, GetRectSize(ACodeText));
end;

function TdxBarCodeQRCodeGenerator.CalculateSize(const AText: string; ASymbology: TdxCustomBarCodeSymbology;
  const AFont: TFont; out AWidth, AHeight: Integer): TdxBarCodeErrorType;
var
  ASize: TSize;
begin
  inherited;
  FSymbology := ASymbology;
  try
    FVersion := Symbology.Version;
    if FVersion = 0 then
      FVersion := GetMinVersion(AText);

    ASize := cxSize(100, 40);
    Result := CheckText(AText);
    if Result = bceNone then
      ASize := GetInternalSize(GetCodeText(AText), AFont);
    AWidth := ASize.cx;
    AHeight := ASize.cy;
  finally
    FSymbology := nil;
  end;
end;

function TdxBarCodeQRCodeGenerator.CheckText(const AText: string): TdxBarCodeErrorType;
var
  I: Integer;
  ADataBitCount: Integer;
begin
  Result := bceNone;
  if FVersion = 0 then
    Result := bceInvalidTextFormat
  else
  begin
    ADataBitCount := FQRCodeDataWordCount[FVersion, Symbology.ErrorCorrectionLevel] * 8 -
      GetCharCountIndicatorLength(FVersion) - 4;
    case Symbology.CompactionMode of
      qrmNumeric:
        begin
          if (Length(AText) div 3) * QRCodeNumberCountToBitCount[3] +
            QRCodeNumberCountToBitCount[Length(AText) mod 3] > ADataBitCount then
              Result := bceInvalidTextFormat
          else
            for I := 1 to Length(AText) do
              if not dxCharInSet(AText[I], ['0'..'9']) then
              begin
                Result := bceInvalidCharacters;
                Break;
              end;
        end;
      qrmAlphaNumeric:
        begin
          if (Length(AText) div 2) * 11 + (Length(AText) mod 2) * 6 > ADataBitCount then
            Result := bceInvalidTextFormat
          else
            for I := 1 to Length(AText) do
              if Pos(AText[I], FSymbolMapQRCodeAlphaNumeric) = 0 then
              begin
                Result := bceInvalidCharacters;
                Break;
              end;
        end;
    else {qrmByte}
      if  TEncoding.UTF8.GetByteCount(AText) * 8 > ADataBitCount then
        Result := bceInvalidTextFormat
    end;
  end;
end;

procedure TdxBarCodeQRCodeGenerator.DrawBars(const ABoundsRect: TRect; const ACodeText: string; const ABarSize: TdxSizeF);
var
  X, Y: Integer;
  AMatrix: Tdx2DBitArray;
  ACurrentPosition: TdxPointF;
  ABarRect: TdxRectF;
begin
  inherited;
  AMatrix := CreateBinaryMatrix(ACodeText);
  ACurrentPosition.X := ABoundsRect.Left + GetLeftOffset * ABarSize.cx;
  ACurrentPosition.Y := ABoundsRect.Top + GetTopOffset * ABarSize.cx;
  for X := 0 to GetMatrixSize - 1 do
  begin
    ACurrentPosition.Y := ABoundsRect.Top + GetTopOffset * ABarSize.cy;
    for Y := 0 to GetMatrixSize - 1 do
    begin
      ABarRect.TopLeft := ACurrentPosition;
      ABarRect.Right := ABarRect.Left + ABarSize.cx;
      ABarRect.Bottom := ABarRect.Top + ABarSize.cy;
      if AMatrix[X, Y] = bit1 then
        FPainter.FillRect(Rect(Round(ABarRect.Left), Round(ABarRect.Top), Round(ABarRect.Right), Round(ABarRect.Bottom)),
          Symbology.ModuleColor);
      ACurrentPosition.Y := ACurrentPosition.Y + ABarSize.cy;
    end;
    ACurrentPosition.X := ACurrentPosition.X + ABarSize.cx;
  end;
end;

procedure TdxBarCodeQRCodeGenerator.DrawText(const ACodeText: string; const ARect: TRect; const
  ABarSize: TdxSizeF; AFont: TFont);
var
  ATextRect: TRect;
begin
  ATextRect := ARect;
  ATextRect.Top := Trunc(ATextRect.Top + (GetMatrixSize + GetTopOffset + GetBottomOffset) * ABarSize.cy);
  ATextRect.Bottom := Round(ARect.Bottom - ABarSize.cx);
  FPainter.DrawText(ATextRect, ACodeText, DT_CENTER or DT_BOTTOM or DT_END_ELLIPSIS, AFont);
end;

function TdxBarCodeQRCodeGenerator.CreateBinaryMatrix(const ACodeText: string;
  AMaskIndex: Integer = -1; AMask: Boolean = True): Tdx2DBitArray;
var
  I: Integer;
  ARegularMatrixMask: Tdx2DBitArray;
  AMaskMatrixMask: Tdx2DBitArray;
begin
  SetLength(Result, GetMatrixSize);
  SetLength(ARegularMatrixMask, GetMatrixSize);
  for I := 0 to GetMatrixSize - 1 do
  begin
    SetLength(Result[I], GetMatrixSize);
    SetLength(ARegularMatrixMask[I], GetMatrixSize);
  end;
  AddSearchPatternsToMatrix(Result, ARegularMatrixMask);
  AddAlignmentPatternsToMatrix(Result, ARegularMatrixMask);
  AddRequiredPatternsToMatrix(Result, ARegularMatrixMask);
  AMaskMatrixMask := GetMaskMatrixMask(ARegularMatrixMask);
  AddDataToMatrix(CreateBinaryCodeFromText(ACodeText), GetDataMatrixMask(AMaskMatrixMask), Result);
  if FVersion >= 7 then
    AddVersionCodeToMatrix(Result);
  if AMask then
  begin
    if AMaskIndex = -1 then
      AssignMask(Result, AMaskMatrixMask, SelectMask(Result, AMaskMatrixMask))
    else
      AssignMask(Result, AMaskMatrixMask, AMaskIndex);
  end;
end;

function TdxBarCodeQRCodeGenerator.GetMinVersion(const AText: string): Integer;
var
  I: Integer;
begin
  Result := Symbology.Version;
  for I := 1 to 40 do
  begin
    if (FQRCodeDataWordCount[I, Symbology.ErrorCorrectionLevel] * 8 - GetCharCountIndicatorLength(I) - 8) >=
      TEncoding.UTF8.GetByteCount(AText) * 8 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TdxBarCodeQRCodeGenerator.SelectMaskRule1(AMatrixWithMask: Tdx2DBitArray; X, Y: Integer; ANewLine: Boolean;
  var AMonochromeModuleCounter: Integer; var ACurrentModuleColor: TdxBit): Integer;
begin
  Result := 0;
  if ANewLine then
  begin
    if AMonochromeModuleCounter >= 5 then
      Result := AMonochromeModuleCounter - 2;

    AMonochromeModuleCounter := 1;
    ACurrentModuleColor := AMatrixWithMask[X][Y];
  end
  else
    if AMatrixWithMask[X][Y] = ACurrentModuleColor then
      Inc(AMonochromeModuleCounter)
    else
    begin
      if AMonochromeModuleCounter >= 5 then
        Result := AMonochromeModuleCounter - 2;

      AMonochromeModuleCounter := 1;
      ACurrentModuleColor := AMatrixWithMask[X][Y];
    end;
end;

function TdxBarCodeQRCodeGenerator.SelectMaskRule2(AMatrixWithMask: Tdx2DBitArray; X, Y: Integer): Integer;
begin
  if (X < GetMatrixSize - 1) and (Y < GetMatrixSize - 1) and
    (AMatrixWithMask[X + 1][Y + 1] = AMatrixWithMask[X][Y]) and
    (AMatrixWithMask[X][Y + 1] = AMatrixWithMask[X][Y]) and
    (AMatrixWithMask[X + 1][Y] = AMatrixWithMask[X][Y]) then
      Result := 3
  else
    Result := 0;
end;

function TdxBarCodeQRCodeGenerator.SelectMaskRule3(AMatrixWithMask: Tdx2DBitArray; X, Y: Integer): Integer;
const
  AShape1: string = '00001011101';
  AShape2: string = '10111010000';
  AShape3: string = '000010111010000';
var
  I: Integer;
begin
  Result := 0;
  if X < GetMatrixSize - 10 then
  begin
    Result := 40;
    for I := 0 to 10 do
    begin
      if AMatrixWithMask[X + I][Y] <> TdxBit(AShape1[I + 1] = '1') then
      begin
        Result := Result - 40;
        Break;
      end;
    end;

    Result := Result + 40;
    for I := 0 to 10 do
    begin
      if AMatrixWithMask[X + I][Y] <> TdxBit(AShape2[I + 1] = '1') then
      begin
        Result := Result - 40;
        Break;
      end;
    end;

  end;
  if X < GetMatrixSize - 14 then
  begin
    Result := Result + 40;
    for I := 0 to 14 do
    begin
      if AMatrixWithMask[X + I][Y] <> TdxBit(AShape3[I + 1] = '1') then
      begin
        Result := Result - 40;
        Break;
      end;
    end;
  end;

  if Y < GetMatrixSize - 10 then
  begin
    Result := Result + 40;
    for I := 0 to 10 do
    begin
      if AMatrixWithMask[X][Y + I] <> TdxBit(AShape1[I + 1] = '1') then
      begin
        Result := Result - 40;
        Break;
      end;
    end;

    Result := Result + 40;
    for I := 0 to 10 do
    begin
      if AMatrixWithMask[X][Y + I] <> TdxBit(AShape2[I + 1] = '1') then
      begin
        Result := Result - 40;
        Break;
      end;
    end;
  end;
  if Y < GetMatrixSize - 14 then
  begin
    Result := Result + 40;
    for I := 0 to 14 do
    begin
      if AMatrixWithMask[X][Y + I] <> TdxBit(AShape3[I + 1] = '1') then
      begin
        Result := Result - 40;
        Break;
      end;
    end;
  end;
end;

function TdxBarCodeQRCodeGenerator.GetBottomOffset: Integer;
begin
  Result := dxBarCodeQRCodeBottomQuietZoneSize;
end;

function TdxBarCodeQRCodeGenerator.GetLeftOffset: Integer;
begin
  Result := dxBarCodeQRCodeLeftQuietZoneSize;
end;

function TdxBarCodeQRCodeGenerator.GetRightOffset: Integer;
begin
  Result := dxBarCodeQRCodeRightQuietZoneSize;
end;

function TdxBarCodeQRCodeGenerator.GetTopOffset: Integer;
begin
  Result := dxBarCodeQRCodeTopQuietZoneSize;
end;

function TdxBarCodeQRCodeGenerator.GetCodeWidth(const ACodeText: string): Integer;
begin
  Result := Symbology.ModuleWidth * (GetMatrixSize + GetLeftOffset + GetRightOffset);
end;

function TdxBarCodeQRCodeGenerator.GetInternalSize(const ACodeText: string; const AFont: TFont): TSize;
begin
  Result.cx := GetCodeWidth(ACodeText);
  Result.cy := DefaultBarHeight * (GetMatrixSize + GetTopOffset + GetBottomOffset);
  if Symbology.ShowText then
  begin
    Result.cx := Max(Result.cx, cxTextWidth(AFont, ACodeText));
    Result.cy := Result.cy + Symbology.ModuleWidth + GetTextHeight(ACodeText, AFont);
  end;
end;

function TdxBarCodeQRCodeGenerator.Paint(APainter: TdxBarCodeCustomPainter; const ABounds: TRect; const AText: string;
  ASymbology: TdxCustomBarCodeSymbology; const AFont: TFont): TdxBarCodeErrorType;
begin
  FSymbology := ASymbology;
  FVersion := TdxBarCodeQRCodeSymbology(Symbology).Version;
  if FVersion = 0 then
    FVersion := GetMinVersion(AText);
  if ASymbology.FitMode = ifmStretch then
    ASymbology.FitMode := ifmProportionalStretch;
  Result := inherited Paint(APainter, ABounds, AText, ASymbology, AFont);
end;

function TdxBarCodeQRCodeGenerator.GetSymbology: TdxBarCodeQRCodeSymbology;
begin
  Result := TdxBarCodeQRCodeSymbology(FSymbology);
end;

procedure TdxBarCodeQRCodeGenerator.AddBinaryData(ABinaryCode: TdxBitArray; const ACodeText: string);
var
  I: Integer;
  APosition: Integer;
begin
  APosition := Length(QRCodeModeIndicator[Symbology.CompactionMode]) + GetCharCountIndicatorLength(FVersion);

  case Symbology.CompactionMode of
    qrmNumeric:
      PopulateBinaryCodeForNumericType(ABinaryCode, ACodeText, APosition);
    qrmAlphaNumeric:
      PopulateBinaryCodeForAlphaNumericType(ABinaryCode, ACodeText, APosition);
    qrmByte:
      PopulateBinaryCodeForByteType(ABinaryCode, ACodeText, APosition);
  end;
  for I := APosition to APosition + Length(QRCodeLimiter) - 1 do
    ABinaryCode[I] := bit0;

  APosition := APosition + Length(QRCodeLimiter);
  if APosition mod 8 > 0 then
  begin
    for I := APosition to APosition + 7 - (APosition mod 8) do
      ABinaryCode[I] := bit0;
    APosition := APosition + 8 - (APosition mod 8);
  end;

  AddNullPattern(ABinaryCode, APosition div 8);
end;

procedure TdxBarCodeQRCodeGenerator.AddIndicatorCharCount(ABinaryCode: TdxBitArray; ACharCount: Integer);
var
  I: Integer;
begin
  for I := Length(QRCodeModeIndicator[Symbology.CompactionMode]) + GetCharCountIndicatorLength(FVersion) - 1 downto
    Length(QRCodeModeIndicator[Symbology.CompactionMode]) do
  begin
    ABinaryCode[I] := TdxBit((ACharCount mod 2) > 0);
    ACharCount := ACharCount div 2;
  end;
end;

procedure TdxBarCodeQRCodeGenerator.AddModeIndicator(ABinaryCode: TdxBitArray);
var
  I: Integer;
begin
  for I := 0 to Length(QRCodeModeIndicator[Symbology.CompactionMode]) - 1 do
    ABinaryCode[I] := TdxBit(QRCodeModeIndicator[Symbology.CompactionMode][I + 1] = '1');
end;

procedure TdxBarCodeQRCodeGenerator.AddNextValue(ABinaryCode: TdxBitArray;
  AValue, APosition, ASymbolCountInValue: Integer);
var
  I: Integer;
begin
  for I := APosition + ASymbolCountInValue - 1 downto APosition do
  begin
    ABinaryCode[I] := TdxBit((AValue mod 2) > 0);
    AValue := AValue div 2;
  end;
end;

procedure TdxBarCodeQRCodeGenerator.AddNullPattern(ABinaryCode: TdxBitArray; AStartWordPosition: Integer);
var
  I, J: Integer;
begin
  for I := AStartWordPosition to GetDataWordCount - 1 do
    for J := 0 to 7 do
      ABinaryCode[I * QRCodeWordLength + J] :=
        TdxBit(QRCodeFillSymbols[Integer(Odd(AStartWordPosition - I))][J + 1] = '1');
end;

procedure TdxBarCodeQRCodeGenerator.CalculateCodeWords(ABinaryCode: TdxBitArray; var ACodeWords: TdxQRCodeBlocks);
var
  I, J, K: Integer;
  AWordInBlockCount: Integer;
  ABitCounter: Integer;
begin
  ABitCounter := 0;
  AWordInBlockCount := GetDataWordCount div GetBlockCount;
  for I := 0 to GetBlockCount - 1 do
  begin
    if I = GetBlockCount - GetDataWordCount mod GetBlockCount then
      Inc(AWordInBlockCount);

    SetLength(ACodeWords[I], AWordInBlockCount);
    for J := 0 to AWordInBlockCount - 1 do
    begin
      ACodeWords[I][J] := 0;
      for K := 0 to 7 do
      begin
        ACodeWords[I][J] := ACodeWords[I][J] + Integer(ABinaryCode[ABitCounter]) * (1 shl (7 - K));
        Inc(ABitCounter);
      end;
    end;
  end;
end;

procedure TdxBarCodeQRCodeGenerator.CalculateErrorCorrectionWords(ACodeWords: TdxQRCodeBlocks;
  var AErrorWords: TdxQRCodeBlocks);
var
  I, J, K: Integer;
  ACurrentElement, ACurrentErrorWord, ADataWordCount, AArraySize: Integer;
begin
  for I := 0 to GetBlockCount - 1 do
  begin
    ADataWordCount := Length(ACodeWords[I]);
    AArraySize := Max(ADataWordCount, GetErrorCorrectionWordInBlockCount);
    SetLength(AErrorWords[I], AArraySize);
    for J := 0 to AArraySize - 1 do
    begin
      if J < ADataWordCount then
        AErrorWords[I][J] := ACodeWords[I][J]
      else
        AErrorWords[I][J] := 0;
    end;
    for J := 0 to ADataWordCount - 1 do
    begin
      ACurrentElement := AErrorWords[I][0];
      if ACurrentElement = 0 then
        ACurrentErrorWord := 0
      else
        ACurrentErrorWord := FQRCodeInverseGaloisField[AErrorWords[I][0]];

      for K := 0 to AArraySize - 1 do
      begin
        if K < AArraySize - 1 then
          AErrorWords[I][K] := AErrorWords[I][K + 1]
        else
          AErrorWords[I][K] := 0;

        if (ACurrentElement > 0) and (K < GetErrorCorrectionWordInBlockCount)  then
          AErrorWords[I][K] := AErrorWords[I][K] xor FQRCodeGaloisField[(ACurrentErrorWord +
            FQRCodeGenerationPolynomial[GetErrorCorrectionWordInBlockCount, K]) mod 255];
      end;
    end;
  end;
end;

function TdxBarCodeQRCodeGenerator.CreateBinaryCodeFromText(const ACodeText: string): TdxBitArray;
var
  ACodeWords: TdxQRCodeBlocks;
  AErrorWords: TdxQRCodeBlocks;
begin
  SetLength(Result, GetDataWordCount * QRCodeWordLength);
  AddModeIndicator(Result);
  AddIndicatorCharCount(Result, TEncoding.UTF8.GetByteCount(ACodeText));
  AddBinaryData(Result, ACodeText);
  CalculateCodeWords(Result, ACodeWords);
  CalculateErrorCorrectionWords(ACodeWords, AErrorWords);
  Result := CreateResultBinaryCode(ACodeWords, AErrorWords);
end;

function TdxBarCodeQRCodeGenerator.CreateResultBinaryCode(ACodeWords, AErrorWords: TdxQRCodeBlocks): TdxBitArray;
var
  I, J, K: Integer;
  AWordCounter: Integer;
  AValue: Integer;
begin
  AWordCounter := 0;
  SetLength(Result, (GetDataWordCount + GetBlockCount * GetErrorCorrectionWordInBlockCount) * 8);
  for I := 0 to Length(ACodeWords[GetBlockCount - 1]) - 1 do
    for J := 0 to GetBlockCount - 1 do
      if I < Length(ACodeWords[J]) then
      begin
        AValue := ACodeWords[J][I];
        for K := 0 to 7 do
        begin
          Result[AWordCounter * 8 + 7 - K] := TdxBit((AValue mod 2) > 0);
          AValue := AValue div 2;
        end;
        Inc(AWordCounter);
      end;
  for I := 0 to GetErrorCorrectionWordInBlockCount - 1 do
    for J := 0 to GetBlockCount - 1 do
    begin
      AValue := AErrorWords[J][I];
      for K := 0 to 7 do
      begin
        Result[AWordCounter * 8 + 7 - K] := TdxBit((AValue mod 2) > 0);
        AValue := AValue div 2;
      end;
      Inc(AWordCounter);
    end;
end;

function TdxBarCodeQRCodeGenerator.GetBlockCount: Integer;
begin
  Result := FQRCodeBlockCount[FVersion, Symbology.ErrorCorrectionLevel];
end;

function TdxBarCodeQRCodeGenerator.GetCharCountIndicatorLength(AVersion: Integer): Integer;
begin
  case Symbology.CompactionMode of
    qrmNumeric:
      begin
        if AVersion < 10 then
          Result := 10
        else
          if AVersion < 27 then
            Result := 12
          else
            Result := 14;
      end;
    qrmAlphaNumeric:
      begin
        if AVersion < 10 then
          Result := 9
        else
          if AVersion < 27 then
            Result := 11
          else
            Result := 13;
      end;
  else
    if AVersion < 10 then
      Result := 8
    else
      Result := 16;
  end;
end;

function TdxBarCodeQRCodeGenerator.GetDataWordCount: Integer;
begin
  Result := FQRCodeDataWordCount[FVersion, Symbology.ErrorCorrectionLevel];
end;

function TdxBarCodeQRCodeGenerator.GetErrorCorrectionWordInBlockCount: Integer;
begin
  Result := FQRCodeErrorCorrectionWordInBlockCount[FVersion, Symbology.ErrorCorrectionLevel];
end;

procedure TdxBarCodeQRCodeGenerator.PopulateBinaryCodeForAlphaNumericType(ABinaryCode: TdxBitArray;
  const ACodeText: string; var APosition: Integer);
var
  I: Integer;
  ATextLength: Integer;
  ACode: Integer;
begin
  ATextLength := Length(ACodeText);
  for I := 0 to ATextLength div 2 - 1 do
  begin
    ACode := (Pos(ACodeText[I * 2 + 1], FSymbolMapQRCodeAlphaNumeric) - 1) * 45 +
      Pos(ACodeText[I * 2 + 2], FSymbolMapQRCodeAlphaNumeric) - 1;
    AddNextValue(ABinaryCode, ACode, APosition, 11);
    APosition := APosition + 11;
  end;
  if ATextLength mod 2 > 0 then
  begin
    ACode := Pos(ACodeText[ATextLength], FSymbolMapQRCodeAlphaNumeric) - 1;
    AddNextValue(ABinaryCode, ACode, APosition, 6);
    APosition := APosition + 6;
  end;
end;

procedure TdxBarCodeQRCodeGenerator.PopulateBinaryCodeForByteType(ABinaryCode: TdxBitArray;
  const ACodeText: string; var APosition: Integer);
var
  I: Integer;
  ABytes: TBytes;
begin
  ABytes := TEncoding.UTF8.GetBytes(ACodeText);
  for I := 0 to TEncoding.UTF8.GetByteCount(ACodeText) - 1 do
  begin
    AddNextValue(ABinaryCode, ABytes[I], APosition, 8);
    APosition := APosition + 8;
  end;
end;

procedure TdxBarCodeQRCodeGenerator.PopulateBinaryCodeForNumericType(ABinaryCode: TdxBitArray;
  const ACodeText: string; var APosition: Integer);
var
  I: Integer;
  ATextLength: Integer;
  AGroupChars: string;
begin
  ATextLength := Length(ACodeText);
  for I := 0 to ATextLength div 3 - 1 do
  begin
    AGroupChars := Copy(ACodeText, I * 3 + 1, 3);
    AddNextValue(ABinaryCode, StrToInt(AGroupChars), APosition, QRCodeNumberCountToBitCount[3]);
    APosition := APosition + QRCodeNumberCountToBitCount[3];
  end;
  if ATextLength mod 3 > 0 then
  begin
    AGroupChars := Copy(ACodeText, (ATextLength div 3) * 3 + 1, ATextLength mod 3);
    AddNextValue(ABinaryCode, StrToInt(AGroupChars), APosition, QRCodeNumberCountToBitCount[Length(AGroupChars)]);
    APosition := APosition + QRCodeNumberCountToBitCount[Length(AGroupChars)];
  end;
end;

procedure TdxBarCodeQRCodeGenerator.AddAlignmentPatternsToMatrix(ARegularMatrix, ARegularMatrixMask: Tdx2DBitArray);

  procedure GenerateSymbol(APosition: TPoint);
  var
    I: Integer;
  begin
    for I := -2 to 2 do
    begin
      ARegularMatrix[APosition.X + I][APosition.Y - 2] := bit1;
      ARegularMatrixMask[APosition.X + I][APosition.Y - 2] := bit1;
      ARegularMatrix[APosition.X + I][APosition.Y + 2] := bit1;
      ARegularMatrixMask[APosition.X + I][APosition.Y + 2] := bit1;
      ARegularMatrix[APosition.X - 2][APosition.Y + I] := bit1;
      ARegularMatrixMask[APosition.X - 2][APosition.Y + I] := bit1;
      ARegularMatrix[APosition.X + 2][APosition.Y + I] := bit1;
      ARegularMatrixMask[APosition.X + 2][APosition.Y + I] := bit1;
    end;
    for I := -1 to 1 do
    begin
      ARegularMatrixMask[APosition.X + I][APosition.Y - 1] := bit1;
      ARegularMatrixMask[APosition.X + I][APosition.Y + 1] := bit1;
      ARegularMatrixMask[APosition.X - 1][APosition.Y + I] := bit1;
      ARegularMatrixMask[APosition.X + 1][APosition.Y + I] := bit1;
    end;
    ARegularMatrix[APosition.X][APosition.Y] := bit1;
    ARegularMatrixMask[APosition.X][APosition.Y] := bit1;
  end;

var
  I, J: Integer;
  X, Y: Integer;
begin
  for I := 0 to 6 do
    for J := 0 to 6 do
    begin
      X := FQRCodeAlignmentElementPositions[FVersion - 1, I];
      Y := FQRCodeAlignmentElementPositions[FVersion - 1, J];
      if (X <> 0) and(Y <> 0) and (ARegularMatrixMask[X][Y] = bit0) then
          GenerateSymbol(Point(X, Y));
    end;
end;

procedure TdxBarCodeQRCodeGenerator.AddCodeLevelAndMaskToMatrix(AMatrix: Tdx2DBitArray; const ACode: string);
var
  I: Integer;
begin
  for I := 0 to 6 do
  begin
    if I = 6 then
      AMatrix[I + 1][8] := TdxBit(ACode[I + 1] = '1')
    else
      AMatrix[I][8] := TdxBit(ACode[I + 1] = '1');

    AMatrix[8][GetMatrixSize - I - 1] := TdxBit(ACode[I + 1] = '1');
  end;
  for I := 0 to 7 do
  begin
    if I > 1 then
      AMatrix[8][7 - I] := TdxBit(ACode[I + 8] = '1')
    else
      AMatrix[8][8 - I] := TdxBit(ACode[I + 8] = '1');

    AMatrix[GetMatrixSize - 8 + I][8] := TdxBit(ACode[I + 8] = '1');
  end;
  AMatrix[8][GetMatrixSize - 8] := bit1;
end;

procedure TdxBarCodeQRCodeGenerator.AddDataToMatrix(ABinaryCode: TdxBitArray; ADataMatrixMask: Tdx2DBitArray;
  ADataMatrix: Tdx2DBitArray);
var
  I, J, K: Integer;
  X, Y: Integer;
  ABitCounter: Integer;
begin
  ABitCounter := 0;
  for I := GetMatrixSize div 2 - 1 downto 0 do
  begin
    for J := 0 to GetMatrixSize - 1 do
    begin
      if Odd(I) then
        Y := GetMatrixSize - J - 1
      else
        Y := J;
      for K := 1 downto 0 do
      begin
        if I < 3 then
          X := I * 2 + K
        else
          X := I * 2 + K + 1;

        if ADataMatrixMask[X, Y] = bit0 then
        begin
          if ABitCounter < Length(ABinaryCode) then
            ADataMatrix[X, Y] := ABinaryCode[ABitCounter]
          else
            ADataMatrix[X, Y] := bit0;

          Inc(ABitCounter);
        end;
      end;
    end;
  end;
end;

procedure TdxBarCodeQRCodeGenerator.AddRequiredPatternsToMatrix(ARegularMatrix, ARegularMatrixMask: Tdx2DBitArray);
var
  I: Integer;
begin
  for I := 0 to Length(ARegularMatrix) - 1 do
  begin
    if ARegularMatrixMask[I][6] = bit0 then
    begin
      ARegularMatrixMask[I][6] := bit1;
      ARegularMatrixMask[6][I] := bit1;
      if not Odd(I) then
      begin
        ARegularMatrix[I][6] := bit1;
        ARegularMatrix[6][I] := bit1;
      end;
    end;
  end;
end;

procedure TdxBarCodeQRCodeGenerator.AddSearchPatternsToMatrix(ARegularMatrix, ARegularMatrixMask: Tdx2DBitArray);

  procedure GenerateSymbol(APosition: TPoint);
  var
    I: Integer;
  begin
    for I := 0 to 6 do
    begin
      ARegularMatrix[APosition.X][I + APosition.Y] := bit1;
      ARegularMatrixMask[APosition.X][I + APosition.Y] := bit1;
      ARegularMatrix[I + APosition.X][APosition.Y] := bit1;
      ARegularMatrixMask[I + APosition.X][APosition.Y] := bit1;
      ARegularMatrix[6 + APosition.X][I + APosition.Y] := bit1;
      ARegularMatrixMask[6 + APosition.X][I + APosition.Y] := bit1;
      ARegularMatrix[I + APosition.X][6 + APosition.Y] := bit1;
      ARegularMatrixMask[I + APosition.X][6 + APosition.Y] := bit1;
    end;
    for I := 1 to 5 do
    begin
      ARegularMatrixMask[1 + APosition.X][I + APosition.Y] := bit1;
      ARegularMatrixMask[I + APosition.X][1 + APosition.Y] := bit1;
      ARegularMatrixMask[5 + APosition.X][I + APosition.Y] := bit1;
      ARegularMatrixMask[I + APosition.X][5 + APosition.Y] := bit1;
    end;
    for I := 2 to 4 do
    begin
      ARegularMatrix[2 + APosition.X][I + APosition.Y] := bit1;
      ARegularMatrixMask[2 + APosition.X][I + APosition.Y] := bit1;
      ARegularMatrix[I + APosition.X][2 + APosition.Y] := bit1;
      ARegularMatrixMask[I + APosition.X][2 + APosition.Y] := bit1;
      ARegularMatrix[4 + APosition.X][I + APosition.Y] := bit1;
      ARegularMatrixMask[4 + APosition.X][I + APosition.Y] := bit1;
      ARegularMatrix[I + APosition.X][4 + APosition.Y] := bit1;
      ARegularMatrixMask[I + APosition.X][4 + APosition.Y] := bit1;
    end;
    ARegularMatrix[3 + APosition.X][3 + APosition.Y] := bit1;
    ARegularMatrixMask[3 + APosition.X][3 + APosition.Y] := bit1;
  end;

var
  I: Integer;
begin
  GenerateSymbol(Point(0, 0));
  GenerateSymbol(Point(Length(ARegularMatrix) - 7, 0));
  GenerateSymbol(Point(0, Length(ARegularMatrix) - 7));
  for I := 0 to 7 do
  begin
    ARegularMatrixMask[7][I] := bit1;
    ARegularMatrixMask[7][Length(ARegularMatrix) - I - 1] := bit1;
    ARegularMatrixMask[Length(ARegularMatrix) - 8][I] := bit1;
    ARegularMatrixMask[I][7] := bit1;
    ARegularMatrixMask[Length(ARegularMatrix) - I - 1][7] := bit1;
    ARegularMatrixMask[I][Length(ARegularMatrix) - 8] := bit1;
  end;
end;

procedure TdxBarCodeQRCodeGenerator.AddVersionCodeToMatrix(AMatrix: Tdx2DBitArray);
var
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 0 to 5 do
    begin
      AMatrix[J][GetMatrixSize - 11 + I] := TdxBit(FQRCodeVersionCode[FVersion][I * 6 + J + 1] = '1');
      AMatrix[GetMatrixSize - 11 + I][J] := TdxBit(FQRCodeVersionCode[FVersion][I * 6 + J + 1] = '1');
    end;
end;

function TdxBarCodeQRCodeGenerator.GetDataMatrixMask(AMaskMatrixMask: Tdx2DBitArray): Tdx2DBitArray;
var
  I, J: Integer;
begin
  SetLength(Result, GetMatrixSize);
  for I := 0 to GetMatrixSize - 1 do
  begin
    SetLength(Result[I], GetMatrixSize);
    for J := 0 to GetMatrixSize - 1 do
      Result[I, J] := AMaskMatrixMask[I, J];
  end;

  for I := 0 to 7 do
  begin
    Result[I, 8] := bit1;
    Result[8, I] := bit1;
    Result[GetMatrixSize - I - 1, 8] := bit1;
    Result[8, GetMatrixSize - I - 1] := bit1;
  end;
  Result[8, 8] := bit1;
end;

function TdxBarCodeQRCodeGenerator.GetMaskMatrixMask(ARegularMatrixMask: Tdx2DBitArray): Tdx2DBitArray;
var
  I, J: Integer;
begin
  SetLength(Result, GetMatrixSize);
  for I := 0 to GetMatrixSize - 1 do
  begin
    SetLength(Result[I], GetMatrixSize);
    for J := 0 to GetMatrixSize - 1 do
      Result[I, J] := ARegularMatrixMask[I, J];
  end;
  if FVersion >= 7 then
    for I := 0 to 5 do
      for J := 8 to 10 do
      begin
        Result[I, GetMatrixSize - J - 1] := bit1;
        Result[GetMatrixSize - J - 1, I] := bit1;
      end;
end;

function TdxBarCodeQRCodeGenerator.GetMatrixSize: Integer;
begin
  Result := 17 + FVersion * 4;
end;

procedure TdxBarCodeQRCodeGenerator.AssignMask(AMatrix, AMaskMatrixMask: Tdx2DBitArray; AMaskIndex: Integer);

  function NeedInvertModule(AMaskIndex, X, Y: Integer): Boolean;
  begin
    case AMaskIndex of
      0:
        Result := (X + Y) mod 2 = 0;
      1:
        Result := Y mod 2 = 0;
      2:
        Result := X mod 3 = 0;
      3:
        Result := (X + Y) mod 3 = 0;
      4:
        Result := (X div 3 + Y div 2) mod 2 = 0;
      5:
        Result := (X * Y) mod 2 + (X * Y) mod 3 = 0;
      6:
        Result := ((X * Y) mod 2 + (X * Y) mod 3) mod 2 = 0;
    else {7}
      Result := ((X * Y) mod 3 + (X + Y) mod 2) mod 2 = 0;
    end;
  end;

var
  X, Y: Integer;
begin
  for X := 0 to GetMatrixSize - 1 do
    for Y := 0 to GetMatrixSize - 1 do
      if (AMaskMatrixMask[X][Y] = bit0) and NeedInvertModule(AMaskIndex, X, Y) then
        AMatrix[X][Y] := TdxBit(not Boolean(AMatrix[X][Y]));

  AddCodeLevelAndMaskToMatrix(AMatrix, FQRCodeMaskAndErrorLevelCode[Symbology.ErrorCorrectionLevel, AMaskIndex]);
end;

function TdxBarCodeQRCodeGenerator.SelectMask(AMatrix, AMaskMatrixMask: Tdx2DBitArray): Integer;
var
  I: Integer;
  X, Y: Integer;
  AMatrixWithMask: Tdx2DBitArray;
  APenaltiesCounter, APenaltyCount: Integer;
  AMonochromeModuleCounterX, AMonochromeModuleCounterY: Integer;
  ACurrentModuleColorX, ACurrentModuleColorY: TdxBit;
  ABlackModuleCounter: Integer;
begin
  Result := 0;
  APenaltyCount := 0;
  SetLength(AMatrixWithMask, GetMatrixSize);
  for I := 0 to GetMatrixSize - 1 do
    SetLength(AMatrixWithMask[I], GetMatrixSize);

  for I := 0 to 7 do
  begin
    APenaltiesCounter := 0;
    AMonochromeModuleCounterX := 0;
    AMonochromeModuleCounterY := 0;
    ACurrentModuleColorX := bit0;
    ACurrentModuleColorY := bit0;
    ABlackModuleCounter := 0;
    for X := 0 to GetMatrixSize - 1 do
      for Y := 0 to GetMatrixSize - 1 do
        AMatrixWithMask[X][Y] := AMatrix[X][Y];

    AssignMask(AMatrixWithMask, AMaskMatrixMask, I);
    for X := 0 to GetMatrixSize - 1 do
      for Y := 0 to GetMatrixSize - 1 do
      begin
        if (I > 0) and (APenaltiesCounter > APenaltyCount) then
          Break;

        APenaltiesCounter := APenaltiesCounter +
          SelectMaskRule1(AMatrixWithMask, X, Y, Y = 0, AMonochromeModuleCounterY, ACurrentModuleColorY) +
          SelectMaskRule1(AMatrixWithMask, Y, X, Y = 0, AMonochromeModuleCounterX, ACurrentModuleColorX) +
          SelectMaskRule2(AMatrixWithMask, X, Y) +
          SelectMaskRule3(AMatrixWithMask, X, Y);

        if AMatrixWithMask[X][Y] = bit1 then
          Inc(ABlackModuleCounter);
      end;

    if Abs(Round(100 * ABlackModuleCounter / (GetMatrixSize * GetMatrixSize) - 50)) > 5 then
      APenaltiesCounter := APenaltiesCounter +
        ((Abs(Round(100 * ABlackModuleCounter / (GetMatrixSize * GetMatrixSize) - 50)) div 5) - 1) * 10;
    if APenaltiesCounter < APenaltyCount then
    begin
      Result := I;
      APenaltyCount := APenaltiesCounter;
    end;
    if I = 0 then
      APenaltyCount := APenaltiesCounter;
  end;
end;

{ TdxCustomBarCodeSymbology }

constructor TdxCustomBarCodeSymbology.Create;
begin
  FShowText := True;
  FGenerator := GetGeneratorClass.Create;
end;

destructor TdxCustomBarCodeSymbology.Destroy;
begin
  FreeAndNil(FGenerator);
  inherited;
end;

procedure TdxCustomBarCodeSymbology.Assign(Source: TPersistent);
begin
  if Source is TdxCustomBarCodeSymbology then
  begin
    FFitMode := TdxCustomBarCodeSymbology(Source).FitMode;
    FModuleColor := TdxCustomBarCodeSymbology(Source).ModuleColor;
    FModuleWidth := TdxCustomBarCodeSymbology(Source).ModuleWidth;
    FShowText := TdxCustomBarCodeSymbology(Source).ShowText;
    FOnChanged := TdxCustomBarCodeSymbology(Source).OnChanged;
  end
  else
    inherited;
end;

function TdxCustomBarCodeSymbology.CalculateSize(const AText: string; const AFont: TFont; out AWidth,
  AHeight: Integer): TdxBarCodeErrorType;
begin
  Result := FGenerator.CalculateSize(AText, Self, AFont, AWidth, AHeight);
end;

function TdxCustomBarCodeSymbology.Paint(APainter: TdxBarCodeCustomPainter; const ABounds: TRect; const AText: string;
  const AFont: TFont): TdxBarCodeErrorType;
begin
  Result := FGenerator.Paint(APainter, ABounds, AText, Self, AFont);
end;

function TdxCustomBarCodeSymbology.Paint(ACanvas: TcxCanvas; const ABounds: TRect; const AText: string;
  const AFont: TFont): TdxBarCodeErrorType;
var
  APainter: TdxBarCodePainter;
begin
  APainter := TdxBarCodePainter.Create;
  try
    APainter.Canvas := ACanvas;
    Result := Paint(APainter, ABounds, AText, AFont);
  finally
    APainter.Free;
  end;
end;

procedure TdxCustomBarCodeSymbology.Changed;
begin
  dxCallNotify(OnChanged, Self);
end;

{ TdxBarCodeSymbologyEAN13 }

function TdxBarCodeEAN13Symbology.GetGeneratorClass: TdxCustomBarCodeGeneratorClass;
begin
  Result := TdxBarCodeEAN13Generator;
end;

{ TdxBarCodeSymbologyEAN8 }

function TdxBarCodeEAN8Symbology.GetGeneratorClass: TdxCustomBarCodeGeneratorClass;
begin
  Result := TdxBarCodeEAN8Generator;
end;

{ TdxBarCodeUPCASymbology }

function TdxBarCodeUPCASymbology.GetGeneratorClass: TdxCustomBarCodeGeneratorClass;
begin
  Result := TdxBarCodeUPCAGenerator;
end;

{ TdxBarCodeUPCESymbology }

function TdxBarCodeUPCESymbology.GetGeneratorClass: TdxCustomBarCodeGeneratorClass;
begin
  Result := TdxBarCodeUPCEGenerator;
end;

{ TdxBarCode128Symbology }

procedure TdxBarCode128Symbology.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxBarCode128Symbology then
    FCharacterSet := TdxBarCode128Symbology(Source).CharacterSet;
end;

function TdxBarCode128Symbology.GetGeneratorClass: TdxCustomBarCodeGeneratorClass;
begin
  Result := TdxBarCode128Generator;
end;

procedure TdxBarCode128Symbology.SetCharacterSet(AValue: TdxBarCode128CharacterSet);
begin
  if FCharacterSet <> AValue then
  begin
    FCharacterSet := AValue;
    Changed;
  end;
end;

{ TdxBarCodeITFTypeSymbology }

constructor TdxBarCodeITFTypeSymbology.Create;
begin
  inherited;
  FChecksum := True;
  FWideNarrowRatio := 3;
end;

procedure TdxBarCodeITFTypeSymbology.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxBarCodeITFTypeSymbology then
  begin
    FChecksum := TdxBarCodeInterleaved2Of5Symbology(Source).Checksum;
    FWideNarrowRatio := TdxBarCodeInterleaved2Of5Symbology(Source).WideNarrowRatio;
  end;
end;

function TdxBarCodeITFTypeSymbology.IsWideNarrowRatioStored: Boolean;
begin
  Result := WideNarrowRatio <> 3;
end;

procedure TdxBarCodeITFTypeSymbology.SetChecksum(AValue: Boolean);
begin
  if FChecksum <> AValue then
  begin
    FChecksum := AValue;
    Changed;
  end;
end;

procedure TdxBarCodeITFTypeSymbology.SetWideNarrowRatio(AValue: Single);
begin
  AValue := Min(3, Max(2, AValue));
  if FWideNarrowRatio <> AValue then
  begin
    FWideNarrowRatio := AValue;
    Changed;
  end;
end;

{ TdxBarCodeInterleaved2Of5Symbology }

function TdxBarCodeInterleaved2Of5Symbology.GetGeneratorClass: TdxCustomBarCodeGeneratorClass;
begin
  Result := TdxBarCodeInterleaved2Of5Generator;
end;

{ TdxBarCode39Symbology }

function TdxBarCode39Symbology.GetGeneratorClass: TdxCustomBarCodeGeneratorClass;
begin
  Result := TdxBarCode39Generator;
end;

{ TdxBarCode39ExtendedSymbology }

function TdxBarCode39ExtendedSymbology.GetGeneratorClass: TdxCustomBarCodeGeneratorClass;
begin
  Result := TdxBarCode39ExtendedGenerator;
end;

{ TdxBarCode93Symbology }

function TdxBarCode93Symbology.GetGeneratorClass: TdxCustomBarCodeGeneratorClass;
begin
  Result := TdxBarCode93Generator;
end;

{ TdxBarCode93ExtendedSymbology }

function TdxBarCode93ExtendedSymbology.GetGeneratorClass: TdxCustomBarCodeGeneratorClass;
begin
  Result := TdxBarCode93ExtendedGenerator;
end;

{ TdxBarCodeMSISymbology }

function TdxBarCodeMSISymbology.GetGeneratorClass: TdxCustomBarCodeGeneratorClass;
begin
  Result := TdxBarCodeMSIGenerator;
end;

{ TdxBarCode11Symbology }

constructor TdxBarCode11Symbology.Create;
begin
  inherited;
  WideNarrowRatio := 2;
end;

function TdxBarCode11Symbology.GetGeneratorClass: TdxCustomBarCodeGeneratorClass;
begin
  Result := TdxBarCode11Generator;
end;

{ TdxBarCodeQRCodeSymbology }

constructor TdxBarCodeQRCodeSymbology.Create;
begin
  inherited;
  FErrorCorrectionLevel := eclM;
  FCompactionMode := qrmByte;
end;

procedure TdxBarCodeQRCodeSymbology.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxBarCodeQRCodeSymbology then
  begin
    FCompactionMode := TdxBarCodeQRCodeSymbology(Source).CompactionMode;
    FErrorCorrectionLevel := TdxBarCodeQRCodeSymbology(Source).ErrorCorrectionLevel;
    FVersion := TdxBarCodeQRCodeSymbology(Source).Version;
  end;
end;

function TdxBarCodeQRCodeSymbology.GetGeneratorClass: TdxCustomBarCodeGeneratorClass;
begin
  Result := TdxBarCodeQRCodeGenerator;
end;

procedure TdxBarCodeQRCodeSymbology.SetCompactionMode(AValue: TdxQRCodeCompactionMode);
begin
  if FCompactionMode <> AValue then
  begin
    FCompactionMode := AValue;
    Changed;
  end;
end;

procedure TdxBarCodeQRCodeSymbology.SetErrorCorrectionLevel(AValue: TdxQRCodeErrorCorrectionLevel);
begin
  if FErrorCorrectionLevel <> AValue then
  begin
    FErrorCorrectionLevel := AValue;
    Changed;
  end;
end;

procedure TdxBarCodeQRCodeSymbology.SetVersion(AValue: Integer);
begin
  if (AValue <= 40) and (AValue >= 0) and (FVersion <> AValue) then
  begin
    FVersion := AValue;
    Changed;
  end;
end;

initialization

finalization
  FreeAndNil(FRegisteredBarCodeSymbologies);

end.
