#include <print>

#include <SDL2/SDL.h>


int main()
{
  SDL_Init(SDL_INIT_TIMER | SDL_INIT_AUDIO | SDL_INIT_JOYSTICK | SDL_INIT_EVENTS);

  for (int index = 0; index < SDL_NumJoysticks(); ++index)
  {
    auto joy = SDL_JoystickOpen(index);
    std::print("Detected joystick {} ({}) - {}\n",
               SDL_JoystickName(joy),
               SDL_JoystickInstanceID(joy),
               SDL_JoystickIsHaptic(joy));

  }

  bool done = false;
  while(!done)
  {
    SDL_Event event;
    while(SDL_PollEvent(&event))
    {
      switch (event.type)
      {
      case SDL_QUIT:
        done = true;
        break;
      case SDL_JOYAXISMOTION:
        std::print("Axis joy:{} axis:{} value:{}\n",
                   event.jaxis.which,
                   event.jaxis.axis,
                   event.jaxis.value);
        break;
      case SDL_JOYBUTTONDOWN:
      case SDL_JOYBUTTONUP:
        std::print("Button joy:{} button:{} state:{}\n",
                   event.jbutton.which,
                   event.jbutton.button,
                   event.jbutton.state);
        break;
      case SDL_JOYHATMOTION:
        std::print("Hat joy:{} hat:{} value:{}\n",
                   event.jhat.which,
                   event.jhat.hat,
                   event.jhat.value);
        break;
      case SDL_JOYDEVICEADDED:
        std::print("Added joy:{}\n",
                   event.jdevice.which);
        break;
      case SDL_JOYDEVICEREMOVED:
        std::print("Removed joy:{}\n",
                   event.jdevice.which);
        break;
      default:
        SDL_JoystickUpdate();
        break;
      }
    }
  }

  SDL_Quit();

  return 0;
}
