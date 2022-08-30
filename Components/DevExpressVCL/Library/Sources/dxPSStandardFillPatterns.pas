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

unit dxPSStandardFillPatterns;

interface

{$I cxVer.inc}

uses
  dxPSFillPatterns;

type
  TdxPSHorizontalFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSVerticalFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSFDiagonalFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSBDiagonalFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSCrossFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  TdxPSDiagCrossFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

implementation

uses
  dxCore, cxClasses, dxPSRes;

{ TdxPSHorizontalFillPattern }

class procedure TdxPSHorizontalFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($FFFF, $0000, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF);
begin
  APattern := Bits;
end;

class function TdxPSHorizontalFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxHorizontalFillPattern);
end;

{ TdxPSVerticalFillPattern }

class procedure TdxPSVerticalFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($FFBF, $FFBF, $FFBF, $FFBF, $FFBF, $FFBF, $FFBF, $FFBF);
begin
  APattern := Bits;
end;

class function TdxPSVerticalFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxVerticalFillPattern);
end;

{ TdxPSFDiagonalFillPattern }

class procedure TdxPSFDiagonalFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($FFFE, $FFFD, $FFFB, $FFF7, $FFEF, $FFDF, $FFBF, $FF7F);
begin
  APattern := Bits;
end;

class function TdxPSFDiagonalFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxFDiagonalFillPattern);
end;

{ TdxPSBDiagonalFillPattern }

class procedure TdxPSBDiagonalFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($FF7F, $FFBF, $FFDF, $FFEF, $FFF7, $FFFB, $FFFD, $FFFE);
begin
  APattern := Bits;
end;

class function TdxPSBDiagonalFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxBDiagonalFillPattern);
end;

{ TdxPSCrossFillPattern }

class procedure TdxPSCrossFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($FFBF, $0000, $FFBF, $FFBF, $FFBF, $FFBF, $FFBF, $FFBF);
begin
  APattern := Bits;
end;

class function TdxPSCrossFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxCrossFillPattern);
end;

{ TdxPSDiagCrossFillPattern }

class procedure TdxPSDiagCrossFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($FF7E, $FFBD, $FFDB, $FFE7, $FFE7, $FFDB, $FFBD, $FF7E);
begin
  APattern := Bits;
end;

class function TdxPSDiagCrossFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxDiagCrossFillPattern);
end;

procedure RegisterPatterns;
begin
  TdxPSHorizontalFillPattern.Register;
  TdxPSVerticalFillPattern.Register;
  TdxPSFDiagonalFillPattern.Register;
  TdxPSBDiagonalFillPattern.Register;
  TdxPSCrossFillPattern.Register;
  TdxPSDiagCrossFillPattern.Register;
end;

initialization
  RegisterPatterns;

end.
