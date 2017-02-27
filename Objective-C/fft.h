#import <Foundation/Foundation.h>
//#import <Foundation/NSString.h>

#include <complex.h>

@interface Fft : NSObject
- (void) fft: (int) log2point andOutVec:(double complex *)xy_out andInVec:(const double complex *)xy_in;
@end

