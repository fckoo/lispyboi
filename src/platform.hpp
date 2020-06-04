#ifndef _PLATFORM_HPP_
#define _PLATFORM_HPP_

#include <filesystem>

namespace plat {
        std::filesystem::path get_working_directory(std::error_code &error);
        void change_directory(const std::filesystem::path &new_dir, std::error_code &error);
        std::filesystem::path get_executable_path();
}

#endif
