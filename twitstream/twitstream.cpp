#include <string>
#include <vector>
#include <iostream>
#include <curl/curl.h>
#include <json_spirit_reader_template.h>
#include <boost/program_options.hpp>
#include <boost/algorithm/string/join.hpp>


size_t callback(void * ptr, size_t size, size_t nmemb, void *)
{
  json_spirit::Value value;
  if(json_spirit::read_string(std::string(static_cast<const char *>(ptr), size * nmemb), value))
  {
    if(value.type() == json_spirit::obj_type)
    {
      for(const auto & item : value.get_obj())
      {
        if(item.name_ == "text" && item.value_.type() == json_spirit::str_type)
        {
          std::cout << item.value_.get_str() << std::endl;
        }
      }
    }
  }
  return size * nmemb;
}

namespace po = boost::program_options;

int main(int argc, char ** argv)
{
  std::string username;
  std::string password;
  std::vector<std::string> keywords;
  po::options_description desc("Options");
  desc.add_options()
    ("help,h", "display this help message")
    ("username,u", po::value<std::string>(&username), "twitter account user name")
    ("password,p", po::value<std::string>(&password), "twitter account password")
    ("keyword,k", po::value<std::vector<std::string> >(&keywords), "keywords to search");
  
  po::variables_map vm;
  po::store
    (po::parse_command_line(argc, argv, desc), 
     vm);
  po::notify(vm);
  
  if(vm.count("help"))
  {
    std::cout << desc << std::endl;
  }
  else
  {
    std::string track = std::string("track=") + boost::join(keywords, ",");
    
    CURL * curl = curl_easy_init();
    curl_easy_setopt(curl, CURLOPT_USERPWD, (username + ":" + password).c_str());
    curl_easy_setopt(curl, CURLOPT_URL, "https://stream.twitter.com/1/statuses/filter.json");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, callback);
    curl_easy_setopt(curl, CURLOPT_POST, 1);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, track.data());
    curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, track.size());
    curl_easy_perform(curl);
    
    curl_easy_cleanup(curl);
  }
}
