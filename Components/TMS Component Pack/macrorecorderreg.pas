{***********************************************************************}
{ TMacroRecorder component                                              }
{ for Delphi & C++Builder                                               }
{ version 1.0                                                           }
{                                                                       }
{ written by                                                            }
{   TMS Software                                                        }
{   Copyright © 2004                                                    }
{   Email : info@tmssoftware.com                                        }
{   Web : http://www.tmssoftware.com                                    }
{***********************************************************************}
unit MacroRecorderReg;

interface

uses
  MacroRecorder, Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS System', [TMacroRecorder]);
end;



end.

