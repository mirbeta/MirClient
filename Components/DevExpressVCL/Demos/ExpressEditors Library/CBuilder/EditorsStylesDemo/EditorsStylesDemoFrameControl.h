//---------------------------------------------------------------------------

#ifndef EditorsStylesDemoFrameControlH
#define EditorsStylesDemoFrameControlH
#include "Controls.hpp"

class TcxFrameControl : public TWinControl {
private:
  TControl* FFramedControl;
protected:
  void __fastcall AdjustFrameRgn();
	DYNAMIC void __fastcall Resize(void);
public:
	__fastcall virtual TcxFrameControl(Classes::TComponent* AOwner);
  void __fastcall FrameControl(TControl* AControl);
  void __fastcall UpdateFrameControlPos();
};
#endif

