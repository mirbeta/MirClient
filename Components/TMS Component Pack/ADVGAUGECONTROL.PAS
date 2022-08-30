{***************************************************************************}
{ IGaugeControl interface                                                   }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2014                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}
unit AdvGaugeControl;

interface

type
  IGaugeControl = interface
    function GetEndPosition: Double;
    function GetFormat: string;
    function GetIsRange: Boolean;
    function GetMinimum: Double;
    function GetMaximum: Double;
    function GetPosition: Double;
    property EndPosition: Double read GetEndPosition;
    property Format: string read GetFormat;
    property IsRange: Boolean read GetIsRange;
    property Minimum: Double read GetMinimum;
    property Maximum: Double read GetMaximum;
    property Position: Double read GetPosition;
  end;

implementation

end.
