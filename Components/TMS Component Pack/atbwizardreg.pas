{*************************************************************************}
{ TMS ToolBars component                                                  }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2006 - 2010                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage caused by the use of this code.                 }
{ The licensed user can use the source code royalty free for building any }
{ compiled application. The complete source code remains property of the  }
{ author and may not be distributed, published, given or sold in any form }
{ as such. No parts of the source code can be included in any other       }
{ component or application without                                        }
{ written authorization of the author.                                    }
{*************************************************************************}

unit ATBWizardReg;

interface

uses
  ATBWizard, Classes, DesignIntf, ToolsAPI, DesignEditors;

procedure Register;

implementation

procedure Register;
begin
  RegisterPackageWizard(TATBFrmWizard.Create);
  RegisterPackageWizard(TATBFrmStyleWizard.Create);
  RegisterPackageWizard(TATBApplicationWizard.Create);
end;

end.
