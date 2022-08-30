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

unit dxPSExcelFillPatterns;

interface

{$I cxVer.inc}

uses
  dxPSFillPatterns;

type
  { TdxPSSolidFillPattern, TdxPSGray50FillPattern are standard fill patterns.
    They are in dxPSFillPatterns.pas }

  TdxPSGray75FillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSGray25FillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSGray125FillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSGray625FillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSHorizontalStripeFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSVerticalStripeFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSReverseDiagonalStripeFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSDiagonalStripeFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSDiagonalCrossHatchFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSThickCrossHatchFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSThinHorizontalStripeFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSThinVerticalStripeFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSThinReverseDiagonalStripeFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSThinDiagonalStripeFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSThinHorizontalCrossHatchFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSThinDiagonalCrossHatchFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

implementation

uses
  dxCore, cxClasses, dxPSRes;

{ TdxPSGray75FillPattern }

class procedure TdxPSGray75FillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($1111, $4444, $1111, $4444, $1111, $4444, $1111, $4444);
begin
  APattern := Bits;
end;

class function TdxPSGray75FillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxGray75FillPattern);
end;

{ TdxPSGray25FillPattern }

class procedure  TdxPSGray25FillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($EEEE, $BBBB, $EEEE, $BBBB, $EEEE, $BBBB, $EEEE, $BBBB);
begin
  APattern := Bits;
end;

class function TdxPSGray25FillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxGray25FillPattern);
end;

{ TdxPSGray125FillPattern }

class procedure TdxPSGray125FillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($FFFF, $EEEE, $FFFF, $BBBB, $FFFF, $EEEE, $FFFF, $BBBB);
begin
  APattern := Bits;
end;

class function TdxPSGray125FillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxGray125FillPattern);
end;

{ TdxPSGray625FillPattern }

class procedure TdxPSGray625FillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($FFFF, $FBFB, $FFFF, $BFBF, $FFFF, $FBFB, $FFFF, $BFBF);
begin
  APattern := Bits;
end;

class function TdxPSGray625FillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxGray625FillPattern);
end;

{ TdxPSHorizontalStripeFillPattern }

class procedure TdxPSHorizontalStripeFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($0000, $0000, $FFFF, $FFFF, $0000, $0000, $FFFF, $FFFF);
begin
  APattern := Bits;
end;

class function TdxPSHorizontalStripeFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxHorizontalStripeFillPattern);
end;

{ TdxPSVerticalStripeFillPattern }

class procedure TdxPSVerticalStripeFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($9999, $9999, $9999, $9999, $9999, $9999, $9999, $9999);
begin
  APattern := Bits;
end;

class function TdxPSVerticalStripeFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxVerticalStripeFillPattern);
end;

{ TdxPSReverseDiagonalStripeFillPattern }

class procedure TdxPSReverseDiagonalStripeFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($CCCC, $6666, $3333, $9999, $CCCC, $6666, $3333, $9999);
begin
  APattern := Bits;
end;

class function TdxPSReverseDiagonalStripeFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxReverseDiagonalStripeFillPattern);
end;

{ TdxPSDiagonalStripeFillPattern }

class procedure TdxPSDiagonalStripeFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($CCCC, $9999, $3333, $6666, $CCCC, $9999, $3333, $6666);
begin
  APattern := Bits;
end;

class function TdxPSDiagonalStripeFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxDiagonalStripeFillPattern);
end;

{ TdxPSDiagonalCrossHatchFillPattern }

class procedure TdxPSDiagonalCrossHatchFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($CCCC, $CCCC, $3333, $3333, $CCCC, $CCCC, $3333, $3333);
begin
  APattern := Bits;
end;

class function TdxPSDiagonalCrossHatchFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxDiagonalCrossHatchFillPattern);
end;

{ TdxPSThickCrossHatchFillPattern }

class procedure TdxPSThickCrossHatchFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($CCCC, $0000, $3333, $0000, $CCCC, $0000, $3333, $0000);
begin
  APattern := Bits;
end;

class function TdxPSThickCrossHatchFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxThickCrossHatchFillPattern);
end;

{ TdxPSThinHorizontalStripeFillPattern }

class procedure TdxPSThinHorizontalStripeFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($0000, $FFFF, $FFFF, $FFFF, $0000, $FFFF, $FFFF, $FFFF);
begin
  APattern := Bits;
end;

class function TdxPSThinHorizontalStripeFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxThinHorizontalStripeFillPattern);
end;

{ TdxPSThinVerticalStripeFillPattern }

class procedure TdxPSThinVerticalStripeFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($BBBB, $BBBB, $BBBB, $BBBB, $BBBB, $BBBB, $BBBB, $BBBB);
begin
  APattern := Bits;
end;

class function TdxPSThinVerticalStripeFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxThinVerticalStripeFillPattern);
end;

{ TdxPSThinReverseDiagonalStripeFillPattern }

class procedure TdxPSThinReverseDiagonalStripeFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($DDDD, $EEEE, $7777, $BBBB, $DDDD, $EEEE, $7777, $BBBB);
begin
  APattern := Bits;
end;

class function TdxPSThinReverseDiagonalStripeFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxThinReverseDiagonalStripeFillPattern);
end;

{ TdxPSThinDiagonalStripeFillPattern }

class procedure TdxPSThinDiagonalStripeFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($EEEE, $DDDD, $BBBB, $7777, $EEEE, $DDDD, $BBBB, $7777);
begin
  APattern := Bits;
end;

class function TdxPSThinDiagonalStripeFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxThinDiagonalStripeFillPattern);
end;

{ TdxPSThinHorizontalCrossHatchFillPattern }

class procedure TdxPSThinHorizontalCrossHatchFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($0000, $BBBB, $BBBB, $BBBB, $0000, $BBBB, $BBBB, $BBBB);
begin
  APattern := Bits;
end;

class function TdxPSThinHorizontalCrossHatchFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxThinHorizontalCrossHatchFillPattern);
end;

{ TdxPSThinDiagonalCrossHatchFillPattern }

class procedure TdxPSThinDiagonalCrossHatchFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($BBBB, $5555, $EEEE, $5555, $BBBB, $5555, $EEEE, $5555);
begin
  APattern := Bits;
end;

class function TdxPSThinDiagonalCrossHatchFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxThinDiagonalCrossHatchFillPattern);
end;

procedure RegisterPatterns;
begin
  TdxPSGray75FillPattern.Register;
  TdxPSGray25FillPattern.Register;
  TdxPSGray125FillPattern.Register;
  TdxPSGray625FillPattern.Register;
  TdxPSHorizontalStripeFillPattern.Register;
  TdxPSVerticalStripeFillPattern.Register;
  TdxPSReverseDiagonalStripeFillPattern.Register;
  TdxPSDiagonalStripeFillPattern.Register;
  TdxPSDiagonalCrossHatchFillPattern.Register;
  TdxPSThickCrossHatchFillPattern.Register;
  TdxPSThinHorizontalStripeFillPattern.Register;
  TdxPSThinVerticalStripeFillPattern.Register;
  TdxPSThinReverseDiagonalStripeFillPattern.Register;
  TdxPSThinDiagonalStripeFillPattern.Register;
  TdxPSThinHorizontalCrossHatchFillPattern.Register;
  TdxPSThinDiagonalCrossHatchFillPattern.Register;
end;

procedure UnregisterPatterns;
begin
  TdxPSThinDiagonalCrossHatchFillPattern.Unregister;
  TdxPSThinHorizontalCrossHatchFillPattern.Unregister;
  TdxPSThinDiagonalStripeFillPattern.Unregister;
  TdxPSThinReverseDiagonalStripeFillPattern.Unregister;
  TdxPSThinVerticalStripeFillPattern.Unregister;
  TdxPSThinHorizontalStripeFillPattern.Unregister;
  TdxPSThickCrossHatchFillPattern.Unregister;
  TdxPSDiagonalCrossHatchFillPattern.Unregister;
  TdxPSDiagonalStripeFillPattern.Unregister;
  TdxPSReverseDiagonalStripeFillPattern.Unregister;
  TdxPSVerticalStripeFillPattern.Unregister;
  TdxPSHorizontalStripeFillPattern.Unregister;
  TdxPSGray625FillPattern.Unregister;
  TdxPSGray125FillPattern.Unregister;
  TdxPSGray25FillPattern.Unregister;
  TdxPSGray75FillPattern.Unregister;
end;

initialization
  RegisterPatterns;

finalization
  UnregisterPatterns;

end.



